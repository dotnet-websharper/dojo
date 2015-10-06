#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.Dojo")
        .VersionFrom("WebSharper")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)

let library =
    bt.WebSharper.Extension("WebSharper.Dojo")
        .SourcesFromProject()
        .Embed(["dojo-config.js"])

let typeProvider =
    bt.WebSharper.Library("WebSharper.Dojo.Provider")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
                r.Assembly "System.Xml"
                r.Assembly "System.Xml.Linq"
            ])

let tests =
    bt.WebSharper.HtmlWebsite("WebSharper.Dojo.Tests")
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

    bt.NuGet.CreatePackage()
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
