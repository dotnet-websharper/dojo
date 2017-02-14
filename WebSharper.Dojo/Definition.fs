namespace WebSharper.DojoExtension

open WebSharper

[<AutoOpen>]
module Pervasives =
    let fail msg =
        let k s = eprintfn "Error: %s" s; failwith s
        Printf.kprintf k msg

module List =

    let rec tryAssoc x = function
        | [] -> None
        | (k, v) :: rest -> if k = x then Some v else tryAssoc x rest

    let rec assoc x = function
        | [] -> fail "assoc %s failed" x
        | (k, v) :: rest -> if k = x then v else assoc x rest

module Json =

    open WebSharper.Core

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

    open WebSharper.Core

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
        {
            Function: Function option
            Members : Members
            Superclass : string option
            Events : Event list
        }

    and Function =
        {
            Parameters : Parameter list
            ReturnType : string
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
            IsStatic : bool
        }

    and Method =
        {
            Name : string
            Parameters : Parameter list
            ReturnType : string
            IsStatic : bool
        }

    and Event =
        {
            Name : string
            Parameters : Parameter list
        }

    let normalizeType (t: string) =
        match t.ToLower() with
        | "undefined" 
        | "object" -> "object"
        | "string" -> "string"
        | "integer" | "int" -> "int"
        | "number" -> "number"
        | "boolean" -> "boolean"
        | "array" -> "array"
        | "void" -> "void"
        | "domnode" -> "domnode"
        | "domevent" | "event" -> "event"
        | "function" -> "function" 
        | _ -> t

    let capitalize (s: string) =
        if System.String.IsNullOrEmpty s then s else s.[0 .. 0].ToUpper() + s.[1 ..]

    let typeOfArr x =
        match x |> Json.asArray with
        | [] -> "void"
        | Json.String x :: _ -> x
        | _ -> "undefined"
        |> normalizeType

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
                    Types = List.assoc "types" p |> Json.asArray |> List.map (Json.asString >> normalizeType)
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
                |> Seq.filter (not << isPrivate)
                |> Seq.map (Json.asObject >> fun o ->
                    let name = List.assoc "name" o |> Json.asString
                    {
                        Name = name
                        Type =
                            match List.assoc "types" o |> typeOfArr with
                            | "object" -> name + "." + name
                            | t -> t
                        IsStatic = (List.assoc "scope" o |> Json.asString) = "normal"
                    })
                |> Seq.distinctBy (fun p -> p.Name)
                |> List.ofSeq
            Methods =
                methods
                |> Seq.filter (not << isPrivate)
                |> Seq.choose (Json.asObject >> fun o ->
                    let name = List.assoc "name" o |> Json.asString
                    if isValid name then
                        let pars, ret = getParamsAndReturns name o
                        Some {
                            Name = name
                            Parameters = pars
                            ReturnType = ret
                            IsStatic = (List.assoc "scope" o |> Json.asString) = "normal"
                        }
                    else None)
                |> Seq.groupBy (fun m -> capitalize m.Name, m.Parameters |> List.map (fun p -> p.Types), m.ReturnType)
                |> Seq.collect (fun (_, g) ->
                    if Seq.length g = 1 then g else
                        g |> Seq.distinctBy (fun m -> m.IsStatic)
                        |> Seq.map (fun m -> if m.IsStatic then { m with Name = m.Name + "S" } else m)
                )
                |> List.ofSeq
            Constructor = ctor
        }

    let getSuperclass props =
        List.tryAssoc "superclass" props
        |> Option.map Json.asString

    let getEvents name props =
        match List.tryAssoc "events" props with
        | Some (Json.Array es) ->
            es |> List.choose (function
                | Json.Object e ->
                    Some {
                        Name = List.assoc "name" e |> Json.asString
                        Parameters =
                            getParams name e
                            |> List.map (fun p ->
                                if p.Name = "event" then
                                    { p with Types = ["DOMEvent"] }
                                else p)
                    }
                | _ -> None)
        | _ -> []

    let getRootElementType name props =
        match List.tryAssoc "type" props |> Option.map Json.asString with
        | Some "instance" | Some "object" ->
            {
                Function = None
                Members = getMembers name props
                Superclass = getSuperclass props
                Events = getEvents name props
            }
            |> Some
        | Some "function" ->
            let pars, ret = getParamsAndReturns name props
            {
                Function = 
                    Some {
                        Parameters = pars
                        ReturnType = ret
                    }
                Members = getMembers name props
                Superclass = getSuperclass props
                Events = getEvents name props
            }
            |> Some
        | Some "constructor" ->
            let pars = Some (getParams name props)
            let m = getMembers name props
            {
                Function = None
                Members = { m with Constructor = if pars.IsSome then pars else m.Constructor }
                Superclass = getSuperclass props
                Events = getEvents name props
            }
            |> Some
        | Some "undefined" -> None // There's only one, "dojo/_firebug/firebug", that seems internal
        | Some "number" -> None // There's only one, "dojo/_base/loader", that seems internal
        | Some t -> fail "%s has unknown type '%A'" name t
        | None -> fail "%s has no type" name

    let rootElements : RootElement list =
        match jsonV with
        | Json.Object vals ->
            vals |> List.choose (function
            | name, Json.Object props ->
                let qualName = name.Split([|'/'; '.'|])
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

open WebSharper.InterfaceGenerator

module Res =

    let Config =
        Resource "Config" "dojo-config.js"

    let Js =
        Resource "Js" "http://ajax.googleapis.com/ajax/libs/dojo/1.10.0/dojo/dojo.js"
        |> Requires [Config]

module Definition =

    open WebSharper.JavaScript.Dom

    let resolveType definedClasses (s: string) =
        match s.ToLower() with
        | "undefined" -> T<obj>
        | "object" -> T<obj>
        | "string" -> T<string>
        | "integer" | "int" -> T<int>
        | "number" -> T<float>
        | "boolean" -> T<bool>
        | "array" -> T<obj[]>
        | "void" -> T<unit>
        | "domnode" -> T<Node>
        | "domevent" | "event" -> T<Event>
        | "function" -> T<JavaScript.Function>
        | _ ->
            match Map.tryFind s definedClasses with
            | None -> T<obj>
            | Some (_, c: CodeModel.Class) -> c.Type

    let resolveToOneType definedClasses = function
        | [] -> resolveType definedClasses "undefined"
        | t::_ -> resolveType definedClasses t

    let WithNonCapitalizedName (c: CodeModel.Class) =
        c.SourceName <- Some (let a = c.Name.Split('.') in a.[a.Length-1])
        c

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
                | [] -> Class parentName |> WithNonCapitalizedName
                | [c, _] -> c
                | (c, _) :: _ ->
                    eprintfn "Warning: several classes with the same name %s" parentName
                    c
            parent
            |=> Nested (toNested (nested |> List.map (fun (c, q) -> c, List.tail q)))
            :> CodeModel.TypeDeclaration)
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
        let staticP, instanceP =
            m.Properties
            |> List.map (fun p ->
                let prop = p.Name =? resolveType definedClasses p.Type
                prop :> CodeModel.IClassMember, p.IsStatic)
            |> List.partition snd
        let staticM, instanceM =
            m.Methods
            |> List.map (fun m ->
                let meth = makeFun definedClasses m.Name m.Parameters m.ReturnType |> fst
                meth :> CodeModel.IClassMember, m.IsStatic)
            |> List.partition snd
        let ctor =
            m.Constructor |> Option.map (fun c ->
                Constructor (fst (makeParameters definedClasses c)) :> CodeModel.IClassMember)
            |> Option.toList
        c
        |+> Static [
            yield! List.map fst staticP
            yield! List.map fst staticM
            yield! ctor
        ]
        |+> Instance [
            yield! List.map fst instanceP
            yield! List.map fst instanceM
        ]

    module Hardcoded =

        let AMD =
            Class "AMD"
            |+> Static [
                Generic - fun t -> "require" => T<string[]>?requires * (t ^-> T<unit>)?callback ^-> T<unit>
                |> WithInline "$global.require($requires, $wsruntime.CreateFuncWithArgs(function(x){return $callback(x.length==1?x[0]:x)}))"
            ]

        let DojoHandler =
            Class "DojoHandler"
            |+> Instance [
                "remove" => T<unit -> unit>
            ]

        let Classes : CodeModel.NamespaceEntity list =
            [
                AMD
                DojoHandler
            ]

    let rootElements =
        let classes =
            DetailsFile.rootElements
            |> List.map (fun e ->
                let name = e.Name
                name, (e, Class(name.Replace("/", ".").Replace("-", "_")) |> WithNonCapitalizedName))
        let definedClasses = Map classes
        classes
        |> List.map (fun (_, (e, c)) ->
            let c =
                match e.Type.Function with
                | Some f ->
                    let invoke =
                        let f, argNames = makeFun definedClasses "invoke" f.Parameters f.ReturnType
                        let fInline =
                            let l =
                                argNames
                                |> String.concat ", $"
                            c.Name + "(" + (if List.isEmpty argNames then "" else "$") + l + ")"
                        f |> WithInline fInline
                    c |+> Static [invoke]
                | None -> c
            let c =
                match e.Type.Superclass with
                | Some sc ->
                    let sc = resolveType definedClasses sc
                    c |=> Inherits sc
                | None -> c
            let c =
                if e.Type.Members.Constructor.IsSome then
                    let configObject =
                        Pattern.Config (e.Name + ".Config") {
                            Required = []
                            Optional =
                                e.Type.Members.Properties
                                |> List.filter (fun p -> not p.IsStatic)
                                |> List.map (fun p ->
                                    p.Name, resolveType definedClasses p.Type)
                        }
                    c
                    |=> Nested [configObject]
                    |+> Static [
                        Constructor configObject
                    ]
                else c
            let c =
                c
                |> addMembers definedClasses e.Type.Members
                |+> Instance (e.Type.Events |> List.map (fun ev ->
                    // event is either "onFooBar" or "_onFooBar";
                    // convert this to "fooBar" or "_foobar".
                    let eventName =
                        if ev.Name.StartsWith "_" then
                            "_" + ev.Name.[3..3].ToLower() + ev.Name.[4..]
                        else
                            ev.Name.[2..2].ToLower() + ev.Name.[3..]
                    let eventArgs = makeParameters definedClasses ev.Parameters |> fst
                    ev.Name => (c -* eventArgs ^-> T<unit>)?callback ^-> Hardcoded.DojoHandler
                    |> WithInteropInline (fun t ->
                        sprintf "$this.on('%s', %s)" eventName (t "callback"))
                    :> CodeModel.IClassMember
                ))
            c, e.QualName)
        |> toNested
        |> List.map (fun c -> c :> CodeModel.NamespaceEntity)

    let Assembly =
        Assembly [
            Namespace "WebSharper.Dojo.Resources" [
                Res.Config
                Res.Js.AssemblyWide()
            ]
            Namespace "WebSharper.Dojo" (Hardcoded.Classes @ rootElements)
        ]

[<Sealed>]
type DojoExtension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<DojoExtension>)>]
do ()
