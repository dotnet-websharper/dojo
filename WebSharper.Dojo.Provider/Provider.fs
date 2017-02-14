namespace WebSharper.Dojo

open System
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

[<TypeProvider>]
type DojoToolkitProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let ctx =
        try ProvidedTypesContext.Create(cfg)
        with _ -> ProvidedTypesContext(List.ofArray cfg.ReferencedAssemblies)

    let thisAssembly = Assembly.GetExecutingAssembly()

    let rootNamespace = "WebSharper.Dojo"

    let objTy = typeof<obj>

    let requireTy = ctx.ProvidedTypeDefinition(thisAssembly, rootNamespace, "Require", None)
    let xhtmlTy = ctx.ProvidedTypeDefinition(thisAssembly, rootNamespace, "XHtml", None)

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

    do
        requireTy.DefineStaticParameters(
            [ctx.ProvidedStaticParameter("requires", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as requires |] ->
                    let requires =
                        requires.Split(',')
                        |> Array.map (fun s -> s.Trim())
                    let members =
                        requires
                        |> Array.filter (fun s -> not (s.Contains("!"))) // filters requires such as dojo/domReady! or dojo/query!css3
                        |> Array.mapi (fun i require ->
                            let ``type`` = findDojoClass require
                            let methods =
                                ``type``.GetConstructors()
                                |> Array.map (fun m ->
                                    let pars =
                                        m.GetParameters()
                                        |> Seq.map (fun par ->
                                            ctx.ProvidedParameter(par.Name, par.ParameterType))
                                        |> List.ofSeq
                                    ctx.ProvidedMethod(require, pars, ``type``, fun (this :: args) ->
                                        let args = Expr.NewArray(objTy, args |> List.map (fun e -> e @?> objTy))
                                        <@@ Inlines.TupleInvoke (%%this) i (%%args : obj[]) @@> @?> ``type``))
                            let types, extraMethods =
                                let meths = ``type``.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
                                if not (Seq.isEmpty meths) then
                                    let ms =
                                        meths
                                        |> Seq.filter (fun m -> not m.IsSpecialName) // remove property getters/setters
                                        |> Seq.map (fun m ->
                                            let pars =
                                                m.GetParameters()
                                                |> Seq.map (fun par ->
                                                    ctx.ProvidedParameter(par.Name, par.ParameterType))
                                                |> List.ofSeq
                                            ctx.ProvidedMethod(m.Name, pars, m.ReturnType, fun (this :: args) ->
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
                                            ctx.ProvidedProperty(p.Name, p.PropertyType, fun [this] ->
                                                let n = uncapitalize p.Name
                                                <@@ Inlines.TupleInvokeGet (%%this) i n @@> @?> ``type``))
                                        |> List.ofSeq
                                    let t = ctx.ProvidedTypeDefinition(require, None)
                                    t.AddMembers(ms)
                                    t.AddMembers(ps)
                                    let m = ctx.ProvidedProperty(require, t, fun [this] -> this @?> ``type``)
                                    [t], [|m|]
                                else [], [||]
                            [
                                yield! Seq.cast<MemberInfo> types
                                yield! Seq.cast<MemberInfo> methods
                                yield! Seq.cast<MemberInfo> extraMethods
                            ])
                    let argsType = ctx.ProvidedTypeDefinition("Requires", None)
                    for membs in members do
                        for m in membs do
                            argsType.AddMember(m)
                    let runCallbackType = FSharpType.MakeFunctionType(argsType, typeof<unit>)
                    let t = ctx.ProvidedTypeDefinition(thisAssembly, rootNamespace, typename, None)
                    t.AddMember(argsType)
                    t.AddMemberDelayed(fun () ->
                        ctx.ProvidedMethod("Run", [ctx.ProvidedParameter("function", runCallbackType)], typeof<unit>,
                            isStatic = true,
                            invokeCode = fun [fn] ->
                                <@@ Inlines.Require requires (%%fn) @@> @?> typeof<unit>)
                    )
                    t
                | _ -> failwith "Unexpected parameter values")
        
        xhtmlTy.DefineStaticParameters(
            [ctx.ProvidedStaticParameter("pathOrXml", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as pathOrXml |] ->
                    let ty = ctx.ProvidedTypeDefinition(thisAssembly, rootNamespace, typename, None)

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

                    ty.AddMember <| ctx.ProvidedConstructor([ ctx.ProvidedParameter("byId", byIdTy) ],
                        fun [ byId ] -> <@@ New %%(widgetQueries byId) @@> @?> ty
                    )
                    
                    ty.AddMembers (
                        widgets |> List.map (fun (i, t) ->
                            let t = findDojoClass t
                            ctx.ProvidedProperty(i, t, fun [ this ] -> <@@ GetField %%(this @?> objTy) i @@> @?> t)
                        )
                    )

                    let jqTy = ctx.ProvidedTypeDefinition("JQuery", None)

                    ty.AddMember jqTy

                    jqTy.AddMembers (
                        ids |> List.map (fun (i, _) ->
                            let hi = "#" + i
                            ctx.ProvidedProperty(i, typeof<JQuery.JQuery>, IsStatic = true,
                                getterCode = fun [] -> <@@ JQuery.JQuery.Of hi @@>)
                        )
                    )

                    ty
                | _ -> failwith "Unexpected parameter values")
        
        this.AddNamespace(rootNamespace, [ requireTy; xhtmlTy ])

[<TypeProviderAssembly>]
do ()
