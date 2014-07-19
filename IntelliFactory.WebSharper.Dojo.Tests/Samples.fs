namespace IntelliFactory.WebSharper.Dojo.Tests

open IntelliFactory.WebSharper

[<AutoOpen>]
module Inlines =

    [<Inline "require($args, $callback)">]
    let require (args: string[]) (callback: 'a -> unit) = ()

module Main =
    open IntelliFactory.WebSharper.Html
    open IntelliFactory.WebSharper.Dojo

    type Req = Require<"dijit/form/ComboBox, dijit/form/CheckBox, dojo/store/Memory, dijit/registry">

    [<JavaScript>]
    let Samples () =
        Div [Attr.Class "claro"] -< [
            Label [Attr.For "myCheckbox"] -< [Text "My checkbox"]
        ]
        |>! OnAfterRender (fun div ->
            Req.Run(fun dojo ->
                let cb =
                    dojo.``dijit/form/CheckBox``(
                        Dijit.Form.CheckBox.Config(
                            Id = "myCheckbox"))
                cb.PlaceAt(div.Body, "first")
                |> ignore
                dojo.``dijit/form/ComboBox``(
                    Dijit.Form.ComboBox.Config(
                        Store =
                            dojo.``dojo/store/Memory``(
                                Dojo.Store.Memory.Config(
                                    Data =
                                        [|
                                            New ["name" => "Entry 1"; "id" => 1]
                                            New ["name" => "Entry 2"; "id" => 2]
                                            New ["name" => "Entry 3"; "id" => 3]
                                        |])),
                        Value = "Entry 2"))
                    .PlaceAt(div.Body, "last")
                |> ignore
                (dojo.``dijit/registry``.ById "myCheckbox" :?> Dijit.Form.CheckBox)
                    .OnClick(fun (_:Dijit.Form.CheckBox) e -> JavaScript.Log e)
            )
        )

type Samples() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body = Main.Samples() :> _


open IntelliFactory.WebSharper.Sitelets

type Action = | Index

module Site =

    open IntelliFactory.Html

    let HomePage =
        Content.PageContent <| fun ctx ->
            { Page.Default with
                Head =
                    [
                        Link [Rel "stylesheet"; HRef "http://ajax.googleapis.com/ajax/libs/dojo/1.10.0/dijit/themes/claro/claro.css"]
                    ]
                Title = Some "WebSharper Dojo Tests"
                Body = [Div [new Samples()]] }

    let Main = Sitelet.Content "/" Index HomePage

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Action.Index]

[<assembly: Website(typeof<Website>)>]
do ()
