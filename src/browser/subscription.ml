open Fmlib_js

type 'm t =
    | None
    | Window of string * 'm Handler.Virtual.t
    | Interval_timer  of int * (Time.t -> 'm)
    | Message of 'm Base.Decode.t
    | Url_request of (Url.t -> 'm)
    | Batch  of 'm t list


let none: 'm t =
    None


let batch (lst: 'm t list): 'm t =
    Batch lst



let on_window (event_type: string) (decode: 'm Base.Decode.t)
    : 'm t
    =
    Window (event_type, Event_flag.(no_stop, no_prevent, decode))



let every (ms: int) (callback: Time.t -> 'm): 'm t =
    Interval_timer (ms, callback)


let on_message (decode: 'm Base.Decode.t): 'm t =
    Message decode


let on_url_request (f: Url.t -> 'm): 'm t =
    Url_request f



let map (f: 'a -> 'b) (sub:'a t): 'b t =
    let rec map =
        function
        | None ->
            None

        | Batch lst ->
            Batch (List.map map lst)

        | Window (event_type, decode) ->
            Window (event_type, Handler.Virtual.map f decode)

        | Interval_timer (millis, g) ->
            Interval_timer (millis, fun time -> f (g time))

        | Message decode ->
            Message Base.Decode.(map f decode)

        | Url_request g ->
            Url_request (fun url -> f (g url))
    in
    map sub



let decode_key_event (f: string -> 'm): 'm Base.Decode.t =
    Base.Decode.(map f (field "key" string))

let decode_mouse_event (f: int -> int -> 'm): 'm Base.Decode.t =
    Base.Decode.(
        let* x = field "clientX" int in
        let* y = field "clientY" int in
        return (f x y)
    )


let on_keydown (f: string -> 'm): 'm t =
    on_window "keydown" (decode_key_event f)

let on_keyup (f: string -> 'm): 'm t =
    on_window "keyup" (decode_key_event f)

let on_mouse_down (f: int -> int -> 'm): 'm t =
    on_window "mousedown" (decode_mouse_event f)

let on_mouse_move (f: int -> int -> 'm): 'm t =
    on_window "mousemove" (decode_mouse_event f)

let on_mouse_up (f: int -> int -> 'm): 'm t =
    on_window "mouseup" (decode_mouse_event f)



let on_resize (f: int -> int -> 'm): 'm t =
    let decode = Base.Decode.(
        let* _ = return () in
        field "currentTarget" (
            let* width  = field "innerWidth" int in
            let* height = field "innerHeight" int in
            return (f width height)
        )
    )
    in
    on_window "resize" decode


let on_visibility_change (f: string -> 'm): 'm t =
    let decode = Base.Decode.(
        field "target" (
            (* The target of the visibility change event is "document" and
               "document" has the "visibilityState" property. *)
            let* state = field "visibilityState" string in
            return (f state)
        )
    )
    in
    on_window "visibilitychange" decode
