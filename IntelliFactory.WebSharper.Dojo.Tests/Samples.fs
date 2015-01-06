namespace IntelliFactory.WebSharper.Dojo.Tests

open IntelliFactory.WebSharper

[<AutoOpen>]
module Inlines =

    [<Inline "require($args, $callback)">]
    let require (args: string[]) (callback: 'a -> unit) = ()

module Main =
    open IntelliFactory.WebSharper.JavaScript
    open IntelliFactory.WebSharper.Html.Client
    open IntelliFactory.WebSharper.Dojo

    type Req = Require<"dijit/form/ComboBox, dijit/form/CheckBox, dojo/store/Memory, dijit/registry, dojo/on, dojo/domReady!">

//    type App = XHtml<"Test.html">

    [<JavaScript>]
    let Samples () =
        Div [Attr.Class "claro"] -< [
            Label [Attr.For "myCheckbox"] -< [Text "My checkbox"]
        ]
        |>! OnAfterRender (fun div ->
            Req.Run(fun d ->
                let cb =
                    d.``dijit/form/CheckBox``(
                        dijit.form.CheckBox.Config(
                            Id = "myCheckbox"))
                cb.PlaceAt(div.Dom, "first")
                |> ignore
                d.``dijit/form/ComboBox``(
                    dijit.form.ComboBox.Config(
                        Store =
                            d.``dojo/store/Memory``(
                                dojo.store.Memory.Config(
                                    Data =
                                        [|
                                            New ["name" => "Entry 1"; "id" => 1]
                                            New ["name" => "Entry 2"; "id" => 2]
                                            New ["name" => "Entry 3"; "id" => 3]
                                        |])),
                        Value = "Entry 2"))
                    .PlaceAt(div.Dom, "last")
                |> ignore
                (d.``dijit/registry``.ById "myCheckbox" :?> dijit.form.CheckBox)
                    .OnClick(fun (_:dijit.form.CheckBox) (e: Dom.Event) -> Console.Log e) |> ignore
                d.``dojo/on``.Invoke(cb, "click", (fun () -> Console.Log "with dojo/on"), null) |> ignore
//                let byId (s: string) = d.``dijit/registry``.ById s
//                byId |> App.newProjectDialog |> ignore
            )
        )

type Samples() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body = Main.Samples() :> _


open IntelliFactory.WebSharper.Sitelets

type Action = | Index

module Site =

    open IntelliFactory.WebSharper.Html.Server

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
