#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"

open System.IO
open System.IO.Compression
open System.Diagnostics

let Base = "dojo-release-1.10.0"

let (+/) x y = Path.Combine(x, y)

do
    // Download dojo zip
    printfn "Downloading %s..." Base
    if Directory.Exists(Base) then
        Directory.Delete(Base, true)
    use s =
        System.Net.HttpWebRequest
            .Create("http://download.dojotoolkit.org/release-1.10.0/dojo-release-1.10.0.zip")
            .GetResponse()
            .GetResponseStream()
    use z = new ZipArchive(s)
    printfn "Extracting %s..." Base
    z.ExtractToDirectory "."

    // Remove the compressed files, replace them with their uncompressed counterpart.
    printfn "Keeping only uncompressed files..."
    [ for f in Directory.GetFiles(Base, "*.js", SearchOption.AllDirectories) do
        if File.Exists(f + ".uncompressed.js") then
            yield f
    ]
    |> Seq.iter (fun f ->
        File.Delete f
        File.Move(f + ".uncompressed.js", f))

    // These are compiled files, we don't want them for documentation.
    printfn "Removing compiled files..."
    File.Delete (Base +/ "dojo" +/ "dojo.js")
    File.Delete (Base +/ "dijit" +/ "dijit.js")
    File.Delete (Base +/ "dijit" +/ "dijit-all.js")

    printfn "Dojo ready to parse."

let JsParseBase = "js-doc-parse-master"

do
    printfn "Downloading %s..." JsParseBase
    if Directory.Exists(JsParseBase) then
        Directory.Delete(JsParseBase, true)
    use s =
        System.Net.HttpWebRequest
            .Create("https://github.com/wkeese/js-doc-parse/archive/master.zip")
            .GetResponse()
            .GetResponseStream()
    use z = new ZipArchive(s)
    printfn "Extracting %s..." JsParseBase
    z.ExtractToDirectory "."
    
    printfn "Configuring %s..." JsParseBase
    File.WriteAllText(JsParseBase +/ "config.js",
        File.ReadAllText(JsParseBase +/ "config.js")
            .Replace("basePath: '../trunk',", sprintf "basePath: '../%s'," Base))

    // TODO: for some reason the following doesn't work, need to run it by hand for now.

    // printfn "Generating doc..."
    // let info =
    //     new ProcessStartInfo(
    //         FileName = JsParseBase +/ "parse.bat",
    //         Arguments = "config=config.js",
    //         UseShellExecute = false,
    //         CreateNoWindow = true)
    // use p = Process.Start(info)
    // p.WaitForExit()
    // File.Copy(JsParseBase +/ "details.json", "IntelliFactory.WebSharper.Dojo" +/ "details.json")
