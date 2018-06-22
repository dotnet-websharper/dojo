namespace WebSharper.Dojo

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml.Linq
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open WebSharper
open WebSharper.JavaScript

module public Inlines =

    [<Inline "$tuple[$idx][$propName]">]
    let TupleInvokeGet (tuple: 'a) (idx: int) (propName: string) = X<'b>

    [<Inline "$tuple[$idx][$methodName].apply($tuple[$idx], $args)">]
    let TupleInvokeMethod (tuple: 'a) (idx: int) (methodName: string) (args: obj[]) = X<'b>

    [<Inline "$tuple[$idx].apply($tuple[$idx], $args)">]
    let TupleInvoke (tuple: 'a) (idx: int) (args: obj[]) = X<'b>

    [<Inline "$func($arg)">]
    let InvokeFunc (func: obj) (arg: obj) = X<obj>

    [<Inline "$obj[$field]">]
    let GetField (obj: obj) (field: string) = X<obj>

    [<JavaScript; Inline>]
    let Require requires (fn: obj -> unit) = AMD.Require(requires, fun o -> fn o)

open Inlines

#nowarn "25"

open ProviderImplementation.ProvidedTypes
open ProviderImplementation
open System.Collections.Concurrent

[<TypeProvider>]
type DojoToolkitProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(cfg)

    let thisAssembly = Assembly.GetExecutingAssembly()

    let rootNamespace = "WebSharper.Dojo"

    let objTy = typeof<obj>

    let requireTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Require", None)
    let xhtmlTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XHtml", None)

    let mutable watcher: FileSystemWatcher = null

    let findDojoClass (name: string) =
        let qualName =
            name.Replace('/', '.').Replace('-', '_').Split('.')
            |> List.ofSeq
        let rec findClass (t: System.Type) = function
            | [] -> t
            | n :: rest ->
                match t.GetNestedType(n) with
                | null -> failwithf "Unknown dojo type: %s" name
                | t -> findClass t rest
        let startClass, nested =
            match qualName with
            | "dojo" :: rest -> typeof<dojo>, rest
            | "dijit" :: rest -> typeof<dijit>, rest
            | "dojox" :: rest -> typeof<dojox>, rest
            | _ -> failwithf "Unknown dojo type: %s" name
        findClass startClass nested

    let alreadyDone = ConcurrentDictionary<string, ProvidedTypeDefinition>()

    do
        requireTy.DefineStaticParameters(
            [ProvidedStaticParameter("requires", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as requires |] ->
                    alreadyDone.GetOrAdd(requires, fun _ ->
                        let requires =
                            requires.Split(',')
                            |> Array.map (fun s -> s.Trim())
                        let members =
                            requires
                            |> Array.mapi (fun i require ->
                                if require.Contains("!") // filters requires such as dojo/domReady! or dojo/query!css3
                                then []
                                else
                                let ``type`` = findDojoClass require
                                let methods =
                                    ``type``.GetConstructors()
                                    |> Array.map (fun m ->
                                        let pars =
                                            m.GetParameters()
                                            |> Seq.map (fun par ->
                                                ProvidedParameter(par.Name, par.ParameterType))
                                            |> List.ofSeq
                                        ProvidedMethod(require, pars, ``type``, fun (this :: args) ->
                                            let args = Expr.NewArray(objTy, args |> List.map (fun e -> e @?> objTy))
                                            <@@ Inlines.TupleInvoke (%%this) i (%%args : obj[]) @@> @?> ``type``))
                                let types, extraMethods =
                                    let meths = ``type``.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
                                    if not (Seq.isEmpty meths) then
                                        let ms =
                                            meths
                                            |> Seq.filter (fun m ->
                                                // remove property getters/setters
                                                not (m.Name.StartsWith "get_")
                                                // This one should work, but for some reason doesn't (Cecil bug?):
                                                // not m.IsSpecialName
                                            )
                                            |> Seq.map (fun m ->
                                                let pars =
                                                    m.GetParameters()
                                                    |> Seq.map (fun par ->
                                                        ProvidedParameter(par.Name, par.ParameterType))
                                                    |> List.ofSeq
                                                ProvidedMethod(m.Name, pars, m.ReturnType, fun (this :: args) ->
                                                    let args = Expr.NewArray(objTy, args |> List.map (fun e -> e @?> objTy))
                                                    let n = uncapitalize m.Name
                                                    if m.Name = "Invoke" then
                                                        <@@ Inlines.TupleInvoke (%%this) i (%%args : obj[]) @@> @?> ``type``
                                                    else
                                                        <@@ Inlines.TupleInvokeMethod (%%this) i n (%%args : obj[]) @@> @?> ``type``))
                                            |> List.ofSeq
                                        let ps =
                                            ``type``.GetProperties(BindingFlags.Static ||| BindingFlags.Public)
                                            |> Seq.map (fun p ->
                                                ProvidedProperty(p.Name, p.PropertyType, fun [this] ->
                                                    let n = uncapitalize p.Name
                                                    <@@ Inlines.TupleInvokeGet (%%this) i n @@> @?> ``type``))
                                            |> List.ofSeq
                                        let t = ProvidedTypeDefinition(require, Some typeof<obj>)
                                        t.AddMembers(ms)
                                        t.AddMembers(ps)
                                        let m = ProvidedProperty(require, t, fun [this] -> this @?> ``type``)
                                        [t], [|m|]
                                    else [], [||]
                                [
                                    yield! Seq.cast<MemberInfo> types
                                    yield! Seq.cast<MemberInfo> methods
                                    yield! Seq.cast<MemberInfo> extraMethods
                                ])
                        let argsType = ProvidedTypeDefinition("Requires", Some typeof<obj>)
                        for membs in members do
                            for m in membs do
                                argsType.AddMember(m)
                        let runCallbackType = FSharpType.MakeFunctionType(argsType, typeof<unit>)
                        let t = ProvidedTypeDefinition(thisAssembly, rootNamespace, typename, Some typeof<obj>)
                        t.AddMember(argsType)
                        t.AddMemberDelayed(fun () ->
                            ProvidedMethod("Run", [ProvidedParameter("function", runCallbackType)], typeof<unit>,
                                isStatic = true,
                                invokeCode = fun [fn] ->
                                    // This is AMD.Require, but using it directly causes an F# compiler bug on netcore:
                                    // error FS1108: The type 'Void' is required here and is unavailable.
                                    // You must add a reference to assembly 'System.Private.CoreLib, Version=4.0.0.0,
                                    // Culture=neutral, PublicKeyToken=7cec85d7bea7798e'
                                    <@@ JS.Inline(
                                            "$global.require($0, $wsruntime.CreateFuncWithArgs(function(x){return $1(x.length==1?x[0]:x)}))",
                                            requires, (%%fn : obj -> unit)
                                        ) @@>
                                    // <@@ Inlines.Require requires %%fn @@>
                                )
                        )
                        t
                    )
                | _ -> failwith "Unexpected parameter values")
        
        xhtmlTy.DefineStaticParameters(
            [ProvidedStaticParameter("pathOrXml", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as pathOrXml |] ->
                    let ty = ProvidedTypeDefinition(thisAssembly, rootNamespace, typename, None)

                    let xml =
                        if pathOrXml.Contains("<") then
                            if watcher <> null then 
                                watcher.Dispose()
                                watcher <- null

                            XDocument.Parse pathOrXml    
                        else 
                            let htmlFile = 
                                if Path.IsPathRooted pathOrXml then pathOrXml 
                                else cfg.ResolutionFolder +/ pathOrXml

                            if cfg.IsInvalidationSupported then
                                if watcher <> null then watcher.Dispose()
                                watcher <-
                                    new FileSystemWatcher(Path.GetDirectoryName htmlFile, Path.GetFileName htmlFile, 
                                        NotifyFilter = (NotifyFilters.LastWrite ||| NotifyFilters.Security ||| NotifyFilters.FileName)
                                    )
                                watcher.Changed.Add <| fun _ -> this.Invalidate()
                                watcher.Deleted.Add <| fun _ -> this.Invalidate()
                                watcher.Renamed.Add <| fun _ -> this.Invalidate()
                                watcher.Created.Add <| fun _ -> this.Invalidate()
                                watcher.EnableRaisingEvents <- true

                            XDocument.Parse(File.ReadAllText htmlFile)

                    let xn n = XName.Get n

                    let byIdTy = typeof<string -> obj>

                    let findDojoClass name =
                        try findDojoClass name
                        with _ -> objTy

                    let ids =
                        xml.Root.Element(xn"body").Descendants()
                        |> Seq.choose (fun e ->
                            let idAttr = e.Attribute(xn"id")
                            if idAttr <> null then
                                Some(
                                    idAttr.Value,
                                    let dojoTypeAttr = e.Attribute(xn"data-dojo-type")
                                    if dojoTypeAttr <> null then Some dojoTypeAttr.Value else None
                                ) 
                            else None
                        )
                        |> List.ofSeq
                    
                    let widgets = ids |> List.choose (function (i, Some t) -> Some(i, t) | _ -> None)
                    
                    let widgetQueries byId =
                        Quotations.Expr.NewArray(
                            typeof<string * obj>,
                            widgets |> List.map (fun (i, _) -> <@@ i, InvokeFunc %%(byId @?> objTy) i @@>)
                        )
                        @?> typeof<seq<string * obj>>

                    ty.AddMember <| ProvidedConstructor([ ProvidedParameter("byId", byIdTy) ],
                        fun [ byId ] -> <@@ New %%(widgetQueries byId) @@> @?> ty
                    )
                    
                    ty.AddMembers (
                        widgets |> List.map (fun (i, t) ->
                            let t = findDojoClass t
                            ProvidedProperty(i, t, fun [ this ] -> <@@ GetField %%(this @?> objTy) i @@> @?> t)
                        )
                    )

                    let jqTy = ProvidedTypeDefinition("JQuery", None)

                    ty.AddMember jqTy

                    jqTy.AddMembers (
                        ids |> List.map (fun (i, _) ->
                            let hi = "#" + i
                            ProvidedProperty(i, typeof<JQuery.JQuery>, isStatic = true,
                                getterCode = fun [] -> <@@ JQuery.JQuery.Of hi @@>)
                        )
                    )

                    ty
                | _ -> failwith "Unexpected parameter values")
        
        this.AddNamespace(rootNamespace, [ requireTy; xhtmlTy ])

    override this.ResolveAssembly(args) =
        let name = AssemblyName(args.Name).Name.ToLowerInvariant()
        let an =
            cfg.ReferencedAssemblies
            |> Seq.tryFind (fun an ->
                Path.GetFileNameWithoutExtension(an).ToLowerInvariant() = name)
        match an with
        | Some f -> Assembly.LoadFrom f
        | None -> null

[<TypeProviderAssembly>]
do ()
