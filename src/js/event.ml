open Js_of_ocaml


class type event =
object
    method stopPropagation: unit -> unit Js.meth
    method preventDefault:  unit -> unit Js.meth
end




type t = event Js.t

let value (e: t): Base.Value.t =
    Obj.magic e

let stop_propagation (e: t): unit =
    e##stopPropagation ()

let prevent_default (e: t): unit =
    e##preventDefault ()


let mouse (typ: string) (options: Base.Value.t): t =
    let make: (Js.js_string Js.t -> Base.Value.t -> event Js.t) Js.constr =
        Js.Unsafe.global##._MouseEvent
    in
    new%js make (Js.string typ) options
