namespace IntelliFactory.WebSharper.Dojo

open System
open System.IO
open System.Reflection
open System.Xml.Linq
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open IntelliFactory.WebSharper


open IntelliFactory.WebSharper.Dojo.ProvidedTypes

module public Inlines =

    [<Inline "$tuple[$idx][$propName]">]
    let TupleInvokeGet (tuple: 'a) (idx: int) (propName: string) = X<'b>


    [<Inline "$tuple[$idx][$methodName].apply($tuple[$idx], $args)">]
    let TupleInvokeMethod (tuple: 'a) (idx: int) (methodName: string) (args: obj[]) = X<'b>

    [<Inline "$tuple[$idx].apply($tuple[$idx], $args)">]
    let TupleInvoke (tuple: 'a) (idx: int) (args: obj[]) = X<'b>

    [<Inline "$func($arg)">]
    let InvokeFunc (func: obj) (arg: obj) = X<obj>

open Inlines

#nowarn "25"

[<TypeProvider>]
type DojoToolkitProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let thisAssembly = Assembly.GetExecutingAssembly()

    let refAssembly name =
        cfg.ReferencedAssemblies
        |> Seq.map (fun an -> Assembly.LoadFrom an)
        |> Seq.tryFind (fun a -> name = a.GetName().Name)
        |> Option.defaultTo null

    do  System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
            refAssembly <| AssemblyName(args.Name).Name
        )
         
    let rootNamespace = "IntelliFactory.WebSharper.Dojo"

    let objTy = typeof<obj>

    let requireTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Require", None)
    let xhtmlTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "XHtml", None)

    let mutable watcher: FileSystemWatcher = null

    let findDojoClass (name: string) =
        let qualName =
            name.Replace('/', '.').Replace('-', '_').Split('.')
            |> Array.map capitalize
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
            [ProvidedStaticParameter("requires", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as requires |] ->
                    let requires =
                        requires.Split(',')
                        |> Array.map (fun s -> s.Trim())
                    let members =
                        requires
                        |> Array.filter (fun s -> not (s.EndsWith("!"))) // filters requires such as dojo/domReady!
                        |> Array.mapi (fun i require ->
                            let ``type`` = findDojoClass require
                            let methods =
                                ``type``.GetConstructors()
                                |> Array.map (fun m ->
                                    let pars =
                                        m.GetParameters()
                                        |> Seq.map (fun par ->
                                            ProvidedParameter(par.Name, par.ParameterType))
                                        |> List.ofSeq
                                    ProvidedMethod(require, pars, ``type``,
                                        InvokeCode = fun (this :: args) ->
                                            let args = Expr.NewArray(objTy, args |> List.map (fun e -> e @?> objTy))
                                            <@@ Inlines.TupleInvoke (%%this) i (%%args : obj[]) @@> @?> ``type``))
                            let types, extraMethods =
                                let meths = ``type``.GetMethods()
                                if (meths |> Seq.exists (fun m -> m.IsStatic)) then
                                    let ms =
                                        meths
                                        |> Seq.filter (fun m -> not m.IsSpecialName) // remove property getters/setters
                                        |> Seq.map (fun m ->
                                            let pars =
                                                m.GetParameters()
                                                |> Seq.map (fun par ->
                                                    ProvidedParameter(par.Name, par.ParameterType))
                                                |> List.ofSeq
                                            ProvidedMethod(m.Name, pars, m.ReturnType,
                                                InvokeCode = fun (this :: args) ->
                                                    let args = Expr.NewArray(objTy, args |> List.map (fun e -> e @?> objTy))
                                                    let n = uncapitalize m.Name
                                                    <@@ Inlines.TupleInvokeMethod (%%this) i n (%%args : obj[]) @@> @?> ``type``))
                                        |> List.ofSeq
                                    let ps =
                                        ``type``.GetProperties()
                                        |> Seq.map (fun p ->
                                            ProvidedProperty(p.Name, p.PropertyType,
                                                GetterCode = fun [this] ->
                                                    let n = uncapitalize p.Name
                                                    <@@ Inlines.TupleInvokeGet (%%this) i n @@> @?> ``type``))
                                        |> List.ofSeq
                                    let t =
                                        ProvidedTypeDefinition(require, None)
                                            .WithMembers(ms)
                                            .WithMembers(ps)
                                    let m =
                                        ProvidedProperty(require, t,
                                            GetterCode = fun [this] -> this @?> ``type``)
                                    [t], [|m|]
                                else [], [||]
                            [
                                yield! Seq.cast<MemberInfo> types
                                yield! Seq.cast<MemberInfo> methods
                                yield! Seq.cast<MemberInfo> extraMethods
                            ])
                    let argsType = ProvidedTypeDefinition("Requires", None)
                    for membs in members do
                        for m in membs do
                            argsType.AddMember(m)
                    let runCallbackType = FSharpType.MakeFunctionType(argsType, typeof<unit>)
                    ProvidedTypeDefinition(thisAssembly, rootNamespace, typename, None)
                        .WithMember(argsType)
                        .WithMemberDelayed(fun () ->
                            ProvidedMethod("Run", [ProvidedParameter("function", runCallbackType)], typeof<unit>,
                                IsStaticMethod = true,
                                InvokeCode = fun [fn] ->
                                    <@@ AMD.Require(requires, (%%fn)) @@> @?> typeof<unit>)
                        )
                | _ -> failwith "Unexpected parameter values")
        
        xhtmlTy.DefineStaticParameters(
            [ProvidedStaticParameter("path", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as path |] ->
                    let ty = ProvidedTypeDefinition(thisAssembly, rootNamespace, typename, None)

                    let htmlFile = 
                        if Path.IsPathRooted path then path 
                        else cfg.ResolutionFolder +/ path

                    if cfg.IsInvalidationSupported then
                        if watcher <> null then 
                            watcher.Dispose()
                        watcher <- new FileSystemWatcher(Path.GetDirectoryName htmlFile, Path.GetFileName htmlFile, EnableRaisingEvents = true)
                        watcher.Changed.Add <| fun _ -> 
                            this.Invalidate()
                    
                    let xml = XDocument.Parse(File.ReadAllText htmlFile)

                    let xn n = XName.Get n

                    let byIdTy = typeof<string -> obj>

                    let findDojoClass name =
                        try findDojoClass name
                        with _ -> typeof<obj>

                    xml.Root.Element(xn"body").Descendants()
                    |> Seq.choose (fun e ->
                        let idAttr = e.Attribute(xn"id")
                        let dojoTypeAttr = e.Attribute(xn"data-dojo-type")
                        if idAttr <> null && dojoTypeAttr <> null then
                            Some (idAttr.Value, dojoTypeAttr.Value)
                        else None     
                    )
                    |> Seq.iter (fun (i, t) ->
                        let returnTy = findDojoClass t
                        ty.AddMember <| ProvidedMethod(i, [ ProvidedParameter("byId", byIdTy) ], returnTy, IsStaticMethod = true
                        ,   InvokeCode = fun [ byId ] -> <@@ InvokeFunc %%(byId @?> objTy) i @@> @?> returnTy   
                        ) 
                    )

                    ty
                | _ -> failwith "Unexpected parameter values")
        
        this.AddNamespace(rootNamespace, [ requireTy; xhtmlTy ])

[<TypeProviderAssembly>]
do ()
