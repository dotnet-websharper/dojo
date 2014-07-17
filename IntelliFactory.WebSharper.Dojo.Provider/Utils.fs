[<AutoOpen>]
module internal IntelliFactory.WebSharper.Dojo.Utils

let ( .- ) table key =
    try Map.find key table
    with _ ->
        failwithf "Key not found: %A in [ %s ]" key
            (table |> Map.toSeq |> Seq.map (fun (k, v) -> sprintf "%A -> %A" k v) |> String.concat "; ") 
let ( .-? ) table key = Map.tryFind key table

let ( +/ ) a b = System.IO.Path.Combine(a, b)

let capitalize (s: string) = s.[0 .. 0].ToUpper() + s.[1 ..]

module Option =
    let defaultTo value option =
        match option with
        | Some v -> v
        | _ -> value

    let defaultWith getter option =
        match option with
        | Some v -> v
        | _ -> getter()

    let fallbackTo optionTo optionFrom =
        match optionFrom with
        | Some _ -> optionFrom
        | _ -> optionTo

