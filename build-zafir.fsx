#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    BuildTool().PackageId("Zafir.Dojo")
        .VersionFrom("Zafir")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)

let library =
    bt.Zafir.Extension("WebSharper.Dojo")
        .SourcesFromProject()
        .Embed(["dojo-config.js"])

let typeProvider =
    bt.Zafir.Library("WebSharper.Dojo.Provider")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
                r.Assembly "System.Xml"
                r.Assembly "System.Xml.Linq"
            ])

let tests =
    bt.Zafir.HtmlWebsite("WebSharper.Dojo.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
                r.Project typeProvider
                r.NuGet("Zafir.Html").Latest(true).ForceFoundVersion().Reference()
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
                Title = Some "Zafir.Dojo"
            }
        )
]
|> bt.Dispatch
