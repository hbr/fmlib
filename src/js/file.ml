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




module List =
struct

  type file = t


  type t = File.fileList Js.t


  let of_value (v: Base.Value.t): t option =
    v |> Obj.magic |> Js.Opt.to_option


  let length (file_list: t): int =
    file_list##.length    

  
  let item (i: int) (file_list: t): file option =
    file_list##item i |> Js.Opt.to_option

end
