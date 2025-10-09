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


let class_ (value: string): 'm t =
    attribute "class" value

let class_list (classes: (string * bool) list): 'm t =
    classes
    |> List.filter_map (fun (c, cond) -> if cond then Some c else None)
    |> String.concat " "
    |> class_


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


let on_mouseenter (m: 'm): 'm t =
    on "mouseenter" (Decoder.return m)


let on_mouseleave (m: 'm): 'm t =
    on "mouseleave" (Decoder.return m)



let decode_key_event (f: string -> 'm): 'm Decoder.t =
    Decoder.(map f (field "key" string))


let on_keydown (f: string -> 'm): 'm t =
    on "keydown" (decode_key_event f)


let on_keyup (f: string -> 'm): 'm t =
    on "keyup" (decode_key_event f)


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




(* Standard attributes *)

let accept (value: string): 'm t =
    attribute "accept" value

let accept_charset (value: string): 'm t =
    attribute "acceptCharset" value

let accesskey (value: string): 'm t =
    attribute "accessKey" value

let action (value: string): 'm t =
    attribute "action" value

let allow (value: string): 'm t =
    attribute "allow" value

let alpha (value: bool): 'm t =
    property "alpha" (Value.bool value)

let alt (value: string): 'm t =
    attribute "alt" value

let autocapitalize (value: string): 'm t =
    attribute "autocapitalize" value

let autocomplete (value: string): 'm t =
    attribute "autocomplete" value

let autoplay (value: bool): 'm t =
    property "autoplay" (Value.bool value)

let capture (value: string): 'm t =
    attribute "capture" value

let checked (value: bool): 'm t =
    property "checked" (Value.bool value)

let cite (value: string): 'm t =
    attribute "cite" value

let colorspace (value: string): 'm t =
    attribute "colorspace" value

let cols (value: int): 'm t =
    attribute "cols" (string_of_int value)

let colspan (value: int): 'm t =
    attribute "colspan" (string_of_int value)

let contenteditable (value: string): 'm t =
    attribute "contenteditable" value

let controls (value: bool): 'm t =
    property "controls" (Value.bool value)

let coords (value: string): 'm t =
    attribute "coords" value

let crossorigin (value: string): 'm t =
    attribute "crossorigin" value

let csp (value: string): 'm t =
    attribute "csp" value

let data (value: string): 'm t =
    attribute "data" value

let datetime (value: string): 'm t =
    attribute "datetime" value

let decoding (value: string): 'm t =
    attribute "decoding" value

let default (value: bool): 'm t =
    property "default" (Value.bool value)

let dir (value: string): 'm t =
    attribute "dir" value

let dirname (value: string): 'm t =
    attribute "dirname" value

let disabled (value: bool): 'm t =
    property "disabled" (Value.bool value)

let download (value: string): 'm t =
    attribute "download" value

let draggable (value: string): 'm t =
    attribute "draggable" value

let enctype (value: string): 'm t =
    attribute "enctype" value

let enterkeyhint (value: string): 'm t =
    attribute "enterkeyhint" value

let elementtiming (value: string): 'm t =
    attribute "elementtiming" value

let for_ (value: string): 'm t =
    attribute "for" value

let form (value: string): 'm t =
    attribute "form" value

let formaction (value: string): 'm t =
    attribute "formaction" value

let formenctype (value: string): 'm t =
    attribute "formenctype" value

let formmethod (value: string): 'm t =
    attribute "formmethod" value

let formnovalidate (value: bool): 'm t =
    property "formNoValidate" (Value.bool value)

let formtarget (value: string): 'm t =
    attribute "formtarget" value

let headers (value: string): 'm t =
    attribute "headers" value

let height (value: int): 'm t =
    attribute "height" (string_of_int value)

let hidden (value: string): 'm t =
    attribute "hidden" value

let high (value: float): 'm t =
    attribute "high" (Printf.sprintf "%g" value)

let href (value: string): 'm t =
    attribute "href" value

let hreflang (value: string): 'm t =
    attribute "hreflang" value

let id (value: string): 'm t =
    attribute "id" value

let inputmode (value: string): 'm t =
    attribute "inputmode" value

let ismap (value: bool): 'm t =
    property "isMap" (Value.bool value)

let itemprop (value: string): 'm t =
    attribute "itemprop" value

let kind (value: string): 'm t =
    attribute "kind" value

let label (value: string): 'm t =
    attribute "label" value

let lang (value: string): 'm t =
    attribute "lang" value

let loading (value: string): 'm t =
    attribute "loading" value

let list (value: string): 'm t =
    attribute "list" value

let loop (value: bool): 'm t =
    property "loop" (Value.bool value)

let low (value: float): 'm t =
    attribute "low" (Printf.sprintf "%g" value)

let max (value: string): 'm t =
    attribute "max" value

let maxlength (value: int): 'm t =
    attribute "maxlength" (string_of_int value)

let minlength (value: int): 'm t =
    attribute "minlength" (string_of_int value)

let media (value: string): 'm t =
    attribute "media" value

let method_ (value: string): 'm t =
    attribute "method" value

let min (value: string): 'm t =
    attribute "min" value

let multiple (value: bool): 'm t =
    property "multiple" (Value.bool value)

let muted (value: bool): 'm t =
    property "muted" (Value.bool value)

let name (value: string): 'm t =
    attribute "name" value

let novalidate (value: bool): 'm t =
    property "noValidate" (Value.bool value)

let open_ (value: bool): 'm t =
    property "open" (Value.bool value)

let optimum (value: float): 'm t =
    attribute "optimum" (Printf.sprintf "%g" value)

let pattern (value: string): 'm t =
    attribute "pattern" value

let ping (value: string): 'm t =
    attribute "ping" value

let placeholder (value: string): 'm t =
    attribute "placeholder" value

let playsinline (value: bool): 'm t =
    property "playsInline" (Value.bool value)

let poster (value: string): 'm t =
    attribute "poster" value

let preload (value: string): 'm t =
    attribute "preload" value

let readonly (value: bool): 'm t =
    property "readOnly" (Value.bool value)

let referrerpolicy (value: string): 'm t =
    attribute "referrerpolicy" value

let rel (value: string): 'm t =
    attribute "rel" value

let required (value: bool): 'm t =
    property "required" (Value.bool value)

let reversed (value: bool): 'm t =
    property "reversed" (Value.bool value)

let role (value: string): 'm t =
    attribute "role" value

let rows (value: int): 'm t =
    attribute "rows" (string_of_int value)

let rowspan (value: int): 'm t =
    attribute "rowspan" (string_of_int value)

let sandbox (value: string): 'm t =
    attribute "sandbox" value

let scope (value: string): 'm t =
    attribute "scope" value

let selected (value: bool): 'm t =
    property "selected" (Value.bool value)

let shape (value: string): 'm t =
    attribute "shape" value

let size (value: int): 'm t =
    attribute "size" (string_of_int value)

let sizes (value: string): 'm t =
    attribute "sizes" value

let span (value: int): 'm t =
    attribute "span" (string_of_int value)

let spellcheck (value: string): 'm t =
    attribute "spellcheck" value

let src (value: string): 'm t =
    attribute "src" value

let srcdoc (value: string): 'm t =
    attribute "srcdoc" value

let srclang (value: string): 'm t =
    attribute "srclang" value

let srcset (value: string): 'm t =
    attribute "srcset" value

let start (value: int): 'm t =
    attribute "start" (string_of_int value)

let step (value: float): 'm t =
    attribute "step" (Printf.sprintf "%g" value)

let tabindex (value: int): 'm t =
    attribute "tabindex" (string_of_int value)

let target (value: string): 'm t =
    attribute "target" value

let title (value: string): 'm t =
    attribute "title" value

let translate (value: string): 'm t =
    attribute "translate" value

let type_ (value: string): 'm t =
    attribute "type" value

let usemap (value: string): 'm t =
    attribute "usemap" value

let value (value: string): 'm t =
    property "value" (Value.string value)

let width (value: int): 'm t =
    attribute "width" (string_of_int value)

let wrap (value: string): 'm t =
    attribute "wrap" value





(* Common style attributes *)

let font_size (size: string): 'm t =
    style "font-size" size

let color (color: string): 'm t =
    style "color" color

let background_color (color: string): 'm t =
    style "background-color" color

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
