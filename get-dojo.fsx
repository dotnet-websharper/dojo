#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"

open System.IO
open System.IO.Compression

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

// TODO: download and run js-doc-parse
