namespace IntelliFactory.WebSharper.Dojo

open System
open System.IO
open System.Reflection
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

    let appTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Require", None)

    do
        appTy.DefineStaticParameters(
            [ProvidedStaticParameter("requires", typeof<string>)],
            fun typename pars ->
                match pars with
                | [| :? string as requires |] ->
                    let requires =
                        requires.Split(',')
                        |> Array.map (fun s -> s.Trim())
                    let members =
                        requires
                        |> Array.mapi (fun i require ->
                            let qualName =
                                require.Replace('/', '.').Replace('-', '_').Split('.')
                                |> Array.map capitalize
                                |> List.ofSeq
                            let ``type`` =
                                let rec findClass (t: System.Type) = function
                                    | [] -> t
                                    | n :: rest ->
                                        match t.GetNestedType(n) with
                                        | null -> failwithf "Unknown require: %s" require
                                        | t -> findClass t rest
                                let startClass, nested =
                                    match qualName with
                                    | "Dojo" :: rest -> typeof<Dojo>, rest
                                    | "Dijit" :: rest -> typeof<Dijit>, rest
                                    | _ -> failwithf "Unknown require: %s" require
                                findClass startClass nested
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
                                            let args = args |> List.map (fun e -> <@@ (%%(e @?> typeof<obj>)) @@>
                                            )
                                            let args = Expr.NewArray(typeof<obj>, args)
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
                                                    let args = args |> List.map (fun e -> <@@ (%%(e @?> typeof<obj>)) @@>)
                                                    let args = Expr.NewArray(typeof<obj>, args)
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
        this.AddNamespace(rootNamespace, [ appTy ])

[<TypeProviderAssembly>]
do ()
