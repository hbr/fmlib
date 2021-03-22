open Js_of_ocaml

type js_string = Js.js_string Js.t

class type xmlHttpRequest =
object
    method open_: js_string -> js_string -> unit Js.meth

    method setRequestHeader: js_string -> js_string -> unit Js.meth

    method send_string: js_string -> unit Js.meth

    method readyState: int Js.readonly_prop
    (*
        0: request not initialized
        1: open has been called
        2: send has been called
        3: loading
        4: complete
    *)

    method status: int Js.readonly_prop
    (* 200: Ok, 403: forbidden, 404: not found. *)

    method statusText: js_string Js.readonly_prop

    method responseText: js_string Js.readonly_prop
end


type t = xmlHttpRequest Js.t


let event_target (req: t): Event_target.t =
    Obj.magic req


let make
        (_method: string)
        (url: string)
        (headers: (string * string) list)
        (body: string)
    : t
    =
    let req: xmlHttpRequest Js.t =
        let request = Js.Unsafe.global##._XMLHttpRequest
        in
        new%js request
    in
    req##open_ (Js.string _method) (Js.string url);
    List.iter
        (fun (name, value) ->
             req##setRequestHeader (Js.string name) (Js.string value)
        )
        headers;
    req##send_string (Js.string body);
    req
