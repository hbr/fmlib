module Local =
struct
    type t

    let string (_: t): string =
        assert false (* nyi *)
end


type t =
    | Raw of string
    | Local of Local.t


let parse (str: string): t option =
    Some (Raw str)


let string: t -> string =
    function
    | Raw str ->
        str
    | Local loc ->
        Local.string loc

let is_page: t -> bool = function
    | Raw _ ->
        true (* MISSING!!! assert false *)
    | Local _ ->
        true
