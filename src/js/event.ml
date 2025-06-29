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

let click: t =
    let mouse_event = Js.Unsafe.global##.MouseEvent in
    new%js mouse_event "click"
