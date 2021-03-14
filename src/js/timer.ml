open Js_of_ocaml

type timer
type interval

let set (f: unit -> unit) (ms: int): timer =
    Js.Unsafe.global##setTimeout f ms

let clear (t: timer): unit =
    Js.Unsafe.global##clearTimeout t


let set_interval (f: unit -> unit) (ms: int): interval =
    Js.Unsafe.global##setInterval f ms


let clear_interval (t: interval): unit =
    Js.Unsafe.global##clearInterval t
