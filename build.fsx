#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    let bt = BuildTool().PackageId("WebSharper.Dojo", "2.5")
    bt.WithFramework(bt.Framework.Net40)

let library =
    bt.WebSharper.Extension("IntelliFactory.WebSharper.Dojo")
        .SourcesFromProject()
        .Embed(["dojo-config.js"])

let typeProvider =
    bt.WebSharper.Library("IntelliFactory.WebSharper.Dojo.Provider")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
            ])

bt.Solution [
    library
    typeProvider

    bt.PackageId("WebSharper.Dojo", "2.5").NuGet.CreatePackage()
        .Description("WebSharper extension for Dojo Toolkit")
        .Add(library)
        .Add(typeProvider)
        .Configure(fun c ->
            { c with
                Authors = ["IntelliFactory"]
                Title = Some "WebSharper.Dojo"
            }
        )
]
|> bt.Dispatch
