open Fmlib_js.Base

type 'msg handler = 'msg Handler.Virtual.t

type 'msg t =
    | Style of string * string
    | Property of string * Value.t
    | Attribute of string * string
    | Handler of string * 'msg handler


let style (key: string) (value: string): 'a t =
    Style (key, value)

let property (key: string) (value: Value.t): 'a t =
    Property (key, value)

let attribute (key: string) (value: string): 'a t =
    Attribute (key, value)

let handler
        (key: string)
        (stop: Event_flag.stop)
        (prevent: Event_flag.prevent)
        (decode: 'msg Decode.t)
    : 'a t
    =
    Handler (key, (stop, prevent, decode))


let map (f: 'a -> 'b): 'a t -> 'b t = function
    | Style _
    | Property _
    | Attribute _ as attr ->
        attr

    | Handler (key, handler)  ->
        Handler (key, Handler.Virtual.map f handler)


let on (key: string) (decode: 'msg Decoder.t): 'msg t =
    handler
        key
        Event_flag.no_stop
        Event_flag.no_prevent
        decode

let on_click (msg: 'msg): 'msg t =
    on "click" (Decoder.return msg)


let decode_key_event (f: string -> 'm): 'm Decoder.t =
    Decoder.(map f (field "key" string))


let on_keydown (f: string -> 'm): 'm t =
    on "keydown" (decode_key_event f)


let on_keyup (f: string -> 'm): 'm t =
    on "keyup" (decode_key_event f)


let on_fileselect (f: File.t list -> 'm): 'm t =
    on "change" Decoder.(map f (field "target" (field "files" file_list)))


(* Styles *)

let font_size (size: string): 'm t =
    style "font-size" size

let color (color: string): 'm t =
    style "color" color

let background_color (color: string): 'm t =
    style "background-color" color

let height (value: string): 'm t =
    style "height" value

let width (value: string): 'm t =
    style "width" value

let margin (value: string): 'm t =
    style "margin" value

let padding (value: string): 'm t =
    style "padding" value

let border_style (value: string): 'm t =
    style "border-style" value

let border_width (value: string): 'm t =
    style "border-width" value

let border_color (value: string): 'm t =
    style "border-color" value


(* Attributes *)

let id (value: string): 'm t =
    attribute "id" value

let class_ (value: string): 'm t =
    attribute "class" value

let href (value: string): 'm t =
    attribute "href" value

let src (value: string): 'm t =
    attribute "src" value

let title (value: string): 'm t =
    attribute "title" value

let value (value: string): 'm t =
    property "value" Value.(string value)

let placeholder (value: string): 'm t =
    attribute "placeholder" value


let on_input (f: string -> 'msg): 'msg t =
    let decode =
        let open Decoder in
        field "target" (field "value" (map f string))
    in
    handler
        "input"
        Event_flag.stop
        Event_flag.no_prevent
        decode
