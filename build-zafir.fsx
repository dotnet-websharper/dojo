#load "tools/includes.fsx"

open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.Dojo")
        .VersionFrom("WebSharper")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun fw -> fw.Net40)

let library =
    bt.WebSharper4.Extension("WebSharper.Dojo")
        .SourcesFromProject()
        .Embed(["dojo-config.js"])

do
    use wc = new System.Net.WebClient()
    let files = 
        [|
            "ProvidedTypes.fsi"
            "ProvidedTypes.fs"
            "AssemblyReader.fs"
            "AssemblyReaderReflection.fs"
            "ProvidedTypesContext.fs"
        |]
    for f in files do
        wc.DownloadFile(
            "https://raw.githubusercontent.com/fsprojects/FSharp.TypeProviders.StarterPack/master/src/" + f,
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "WebSharper.Dojo.Provider", f)
        )

let typeProvider =
    bt.WebSharper4.Library("WebSharper.Dojo.Provider")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
                r.Assembly "System.Xml"
                r.Assembly "System.Xml.Linq"
            ])

let tests =
    bt.WebSharper4.HtmlWebsite("WebSharper.Dojo.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project library
                r.Project typeProvider
                r.NuGet("WebSharper.Html").Latest(true).ForceFoundVersion().Reference()
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
