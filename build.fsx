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
                r.Assembly "System.Xml"
                r.Assembly "System.Xml.Linq"
            ])

let tests =
    bt.WebSharper.HtmlWebsite("IntelliFactory.WebSharper.Dojo.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
                r.Project typeProvider
            ])

bt.Solution [
    library
    typeProvider
    tests

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
