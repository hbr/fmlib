open Js_of_ocaml

type js_string = Js.js_string Js.t

class type event_target =
object
    method addEventListener:    js_string -> ('a -> unit) -> unit Js.meth
    method removeEventListener: js_string -> ('a -> unit) -> unit Js.meth
end


type t = event_target Js.t

let add (name: string) (handler: Event.t -> unit) (tgt: t): unit =
    tgt##addEventListener (Js.string name) handler

let remove (name: string) (handler: Event.t -> unit) (tgt: t): unit =
    tgt##removeEventListener (Js.string name) handler
