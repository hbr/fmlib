open Js_of_ocaml


type t = File.file Js.t


let name (file: t): string =
    file##.name |> Js.to_string


let media_type (file: t): string option =
    match file##._type |> Js.to_string with
    | "" ->
        None
    | s ->
        Some s

  
let size (file: t): int =
    file##.size

