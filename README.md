# WebSharper.Dojo

This is the WebSharper binding for [Dojo Toolkit](http://dojotoolkit.org/).

## WIG project

The classes are generated using WIG by parsing the file `details.json`.

Here is how you can re-generate `details.json`:

* Install Node.js

* Run `get-dojo.fsx` to download, extract and cleanup dojo and the
  documentation generation tool, then:

    cd js-doc-parse-master
    parse.bat config=config.js
    copy details.json ..\IntelliFactory.WebSharper.Dojo\

## Type provider

The type provider automates the AMD (asynchronous module definition). Check the
test project to see how to use it.