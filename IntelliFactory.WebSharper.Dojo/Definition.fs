﻿namespace IntelliFactory.WebSharper.DojoExtension

open IntelliFactory.WebSharper.Dom
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.Html5.WebGL

[<AutoOpen>]
module Pervasives =
    let fail msg =
        let k s = eprintfn "%s" s; failwith s
        Printf.kprintf k msg

module List =

    let rec tryAssoc x = function
        | [] -> None
        | (k, v) :: rest -> if k = x then Some v else tryAssoc x rest

    let rec assoc x = function
        | [] -> fail "assoc %s failed" x
        | (k, v) :: rest -> if k = x then v else assoc x rest

module Json =

    open IntelliFactory.WebSharper.Core

    let asString = function
        | Json.String s -> s
        | _ -> fail "not a string"

    let asObject = function
        | Json.Object s -> s
        | _ -> fail "not an object"

    let asArray = function
        | Json.Array a -> a
        | _ -> fail "not an array"

module DetailsFile =

    open IntelliFactory.WebSharper.Core

    let jsonV =
        try
            let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "details.json")
            use f = System.IO.File.OpenRead(path)
            use r = new System.IO.StreamReader(f)
            Json.Read r
        with _ ->
            fail "File doesn't exist or is not valid JSON: details.json"

    let isValid =
        let re = System.Text.RegularExpressions.Regex("[^a-zA-Z0-9_]", System.Text.RegularExpressions.RegexOptions.Compiled)
        fun s -> not(re.IsMatch(s))

    type RootElement =
        {
            Type : Type
            Name : string
            QualName : string list
        }

    and Type =
        | Function of Function
        | Object of Members
        | Unknown

    and Function =
        {
            Parameters : Parameter list
            ReturnType : string
            Members : Members
        }

    and Parameter =
        {
            Name : string
            Types : string list
            Required : bool
        }

    and Members =
        {
            Properties : Property list
            Methods : Method list
            Constructor : Parameter list option
        }

    and Property =
        {
            Name : string
            Type : string
        }

    and Method =
        {
            Name : string
            Parameters : Parameter list
            ReturnType : string
        }

    let typeOfArr x =
        match x |> Json.asArray with
        | [] -> "void"
        | Json.String x :: _ -> x
        | _ -> "undefined"

    let isPrivate o =
        match List.tryAssoc "tags" (Json.asObject o) with
        | Some (Json.Array a) ->
            a |> List.exists (function
                | Json.String "private" | Json.String "protected" -> true
                | _ -> false)
        | _ -> false

    let getParams name o =
        defaultArg (List.tryAssoc "parameters" o) (Json.Array [])
        |> Json.asArray
        |> List.map (function
            | Json.Object p ->
                {
                    Name = List.assoc "name" p |> Json.asString
                    Types = List.assoc "types" p |> Json.asArray |> List.map Json.asString
                    Required = (List.assoc "usage" p |> Json.asString) = "required"
                }
            | _ -> fail "%s parameters is not an object" name)

    let getParamsAndReturns name o =
        let pars = getParams name o
        let ret = List.assoc "returnTypes" o |> typeOfArr
        pars, ret

    let getMembers name props =
        let ctors, methods =
            defaultArg (List.tryAssoc "methods" props) (Json.Array [])
            |> Json.asArray
            |> List.partition (Json.asObject >> fun o ->
                List.assoc "name" o |> Json.asString = "constructor")
        let ctor =
            match ctors with
            | [] -> None
            | [Json.Object o] ->
                match List.assoc "returnTypes" o |> Json.asArray with
                | [Json.String "object"] | [] ->
                    Some (getParams name o)
                | _ -> None
            | _ -> fail "Several constructors in %s" name
        {
            Properties =
                defaultArg (List.tryAssoc "properties" props) (Json.Array [])
                |> Json.asArray
                |> List.filter (not << isPrivate)
                |> List.map (Json.asObject >> fun o ->
                    let name = List.assoc "name" o |> Json.asString
                    {
                        Name = name
                        Type =
                            match List.assoc "types" o |> typeOfArr with
                            | "object" -> name + "." + name
                            | t -> t
                    })
            Methods =
                methods
                |> List.filter (not << isPrivate)
                |> List.choose (Json.asObject >> fun o ->
                    let name = List.assoc "name" o |> Json.asString
                    if isValid name then
                        let pars, ret = getParamsAndReturns name o
                        Some {
                            Name = name
                            Parameters = pars
                            ReturnType = ret
                        }
                    else None)
            Constructor = ctor
        }

    let getRootElementType name props =
        match List.tryAssoc "type" props |> Option.map Json.asString with
        | Some "instance" | Some "object" ->
            getMembers name props
            |> Object
            |> Some
        | Some "function" ->
            let pars, ret = getParamsAndReturns name props
            Function {
                Parameters = pars
                ReturnType = ret
                Members = getMembers name props
            }
            |> Some
        | Some "undefined" -> Some Unknown
        | Some "constructor" ->
            let pars = Some (getParams name props)
            let m = getMembers name props
            { m with Constructor = if pars.IsSome then pars else m.Constructor }
            |> Object
            |> Some
        | Some "number" -> None // There's only one, "dojo/_base/loader", that seems internal
        | Some t -> fail "%s has unknown type '%A'" name t
        | None -> fail "%s has no type" name

    let rootElements : RootElement list =
        match jsonV with
        | Json.Object vals ->
            vals |> List.choose (function
            | name, Json.Object props ->
                let qualName = name.Split([|'/'; '.'|])
//                if qualName |> Seq.exists (fun s -> s.StartsWith("_")) then
//                    None // Don't export internal classes
//                else
                getRootElementType name props
                |> Option.map (fun t ->
                    {
                        Type = t
                        Name = name
                        QualName = qualName |> List.ofArray
                    })

            | name, _ -> fail "%s is not an object" name
            )
        | _ ->
            fail "Parsed JSON is not an object"

open IntelliFactory.WebSharper.InterfaceGenerator

module Res =

    let Config =
        Resource "Config" "dojo-config.js"

    let Js =
        Resource "Js" "http://ajax.googleapis.com/ajax/libs/dojo/1.10.0/dojo/dojo.js"
        |> Requires [Config]

module Definition =

    open IntelliFactory.WebSharper.Dom

    let resolveType definedClasses (s: string) =
        match s.ToLower() with
        | "undefined" -> T<obj>
        | "object" -> T<obj>
        | "string" -> T<string>
        | "integer" -> T<int>
        | "number" -> T<float>
        | "boolean" -> T<bool>
        | "array" -> T<obj[]>
        | "void" -> T<unit>
        | "domnode" -> T<Node>
        | _ ->
            match Map.tryFind s definedClasses with
            | None -> T<obj>
            | Some (_, c: CodeModel.Class) -> c.Type

    let rec toNested (classes: (CodeModel.Class * string list) seq) =
        classes
        |> Seq.groupBy (snd >> List.head)
        |> Seq.map (fun (parentName, classes) ->
            let parent, nested =
                classes
                |> List.ofSeq
                |> List.partition (fun (_, qualName) -> qualName.Length = 1)
            let parent =
                match parent with
                | [] -> Class parentName
                | [c, _] -> c
                | (c, _) :: _ ->
                    eprintfn "Warning: several classes with the same name %s" parentName
                    c
            parent
            |=> Nested (toNested (nested |> List.map (fun (c, q) -> c, List.tail q))))
        |> List.ofSeq

    let makeParameters definedClasses (pars: DetailsFile.Parameter list) =
        let argTypes, revArgNames =
            ((Type.Parameters.Empty, []), pars)
            ||> List.fold (fun (pars, names) par ->
                let p =
                    par.Types
                    |> List.map (resolveType definedClasses)
                    |> List.reduce (+)
                pars * (if par.Required then p?(par.Name) else !?p?(par.Name)), par.Name :: names)
        argTypes, List.rev revArgNames

    let makeFun definedClasses name (pars: DetailsFile.Parameter list) (ret: string) =
        let argTypes, argNames = makeParameters definedClasses pars
        (name => argTypes ^-> resolveType definedClasses ret), argNames

    let addMembers definedClasses (m: DetailsFile.Members) c =
        c
        |+> Protocol (m.Properties |> List.choose (fun p ->
            Some (p.Name =? resolveType definedClasses p.Type :> _)))
        |+> Protocol (m.Methods |> List.choose (fun m ->
            Some (makeFun definedClasses m.Name m.Parameters m.ReturnType |> fst :> _)))
        |+> (m.Constructor |> Option.map (fun c ->
                Constructor (fst (makeParameters definedClasses c)) :> CodeModel.IClassMember)
            |> Option.toList)

    let rootElements =
        let classes =
            DetailsFile.rootElements
            |> List.map (fun e ->
                let name = e.Name.Replace("/", ".")
                name, (e, Class name))
        let definedClasses = Map classes
        classes
        |> List.map (fun (_, (e, c)) ->
            let c =
                match e.Type with
                | DetailsFile.Object m ->
                    c
                    |> addMembers definedClasses m
                | DetailsFile.Function f ->
                    let invoke =
                        let f, argNames = makeFun definedClasses "invoke" f.Parameters f.ReturnType
                        let fInline =
                            let l =
                                argNames
                                |> String.concat ", $"
                            "$this(" + (if List.isEmpty argNames then "" else "$") + l + ")"
                        f //|> WithInline fInline
                    c
                    |+> Protocol [invoke]
                    |> addMembers definedClasses f.Members
                | DetailsFile.Unknown ->
                    c
            c, e.QualName)
//        |> List.map (fun s ->
//            printfn "%A" (snd s)
//            s)
        |> toNested
        |> List.map (fun c -> c :> CodeModel.NamespaceEntity)

    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.Dojo.Resources" [
                Res.Config
                Res.Js.AssemblyWide()
            ]
            Namespace "IntelliFactory.WebSharper.Dojo" rootElements
        ]

[<Sealed>]
type DojoExtension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<DojoExtension>)>]
do ()
