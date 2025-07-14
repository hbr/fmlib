type file = File.t

open Js_of_ocaml

type js_string = Js.js_string Js.t

type blob = File.blob Js.t

class type xmlHttpRequest =
object
    method open_: js_string -> js_string -> unit Js.meth

    method setRequestHeader: js_string -> js_string -> unit Js.meth

    method send_string: js_string -> unit Js.meth

    method send_blob: blob -> unit Js.meth

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
    req


let make_text
        (_method: string)
        (url: string)
        (headers: (string * string) list)
        (text: string)
    : t
    =
    let req = make _method url headers in
    req##send_string (Js.string text);
    req


let make_file
        (_method: string)
        (url: string)
        (headers: (string * string) list)
        (file: file)
    : t
    =
    let req = make _method url headers in
    req##send_blob (Obj.magic file);
    req


let ready_state (req: t): int =
    req##.readyState


let status (req: t): int =
    req##.status


let response_text_value (req: t): Base.Value.t =
    Obj.magic req##.responseText


let response_text_string (req: t): string =
    Js.to_string req##.responseText
