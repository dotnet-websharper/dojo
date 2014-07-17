namespace IntelliFactory.WebSharper.Dojo

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices

open IntelliFactory.WebSharper.Dojo.ProvidedTypes

module public Inlines =
    open IntelliFactory.WebSharper

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

    let appTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Toolkit", None) 

    do
        this.AddNamespace(rootNamespace, [ appTy ])

[<TypeProviderAssembly>]
do ()
