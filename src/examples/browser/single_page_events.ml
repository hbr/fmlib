open Fmlib_browser

type event =
    | Key_down of string
    | Key_up   of string
    | Mouse_down of int * int
    | Mouse_move of int * int
    | Mouse_up   of int * int
    | Resize     of int * int
    | Visibility_change of string

type state = event option

type msg =
    | Got_key_down of string
    | Got_key_up   of string
    | Got_mouse_down of int * int
    | Got_mouse_move of int * int
    | Got_mouse_up   of int * int
    | Got_resize     of int * int
    | Got_visibility_change of string


let init: state * msg Command.t =
    (None, Command.none)


let update (_state: state) (msg: msg): state * msg Command.t =
    let state =
        match msg with
        | Got_key_down key ->
            Key_down key
        | Got_key_up key ->
            Key_up key
        | Got_mouse_down (x, y) ->
            Mouse_down (x, y)
        | Got_mouse_move (x, y) ->
            Mouse_move (x, y)
        | Got_mouse_up (x, y) ->
            Mouse_up (x, y)
        | Got_resize (x, y) ->
            Resize (x, y)
        | Got_visibility_change str ->
            Visibility_change str
    in
    (Some state, Command.none)


let view (state: state): msg Html.t =
    let open Html in
    let view_xy str x y =
        let xstr = string_of_int x in
        let ystr = string_of_int y in
        div [] List.(map text [str; ": "; xstr; "/"; ystr])
    in
    let view_mouse str x y =
        view_xy ("mouse " ^ str) x y
    in
    let event_html =
        match state with
        | None ->
            div [] [text "no event yet occurred"]

        | Some (Key_down key) ->
            div [] [text "key down: "; text "\""; text key; text "\""]

        | Some (Key_up key) ->
            div [] [text "key up: "; text "\""; text key; text "\""]

        | Some (Mouse_down (x, y)) ->
            view_mouse "down" x y

        | Some (Mouse_move (x, y)) ->
            view_mouse "move" x y

        | Some (Mouse_up (x, y)) ->
            view_mouse "up" x y

        | Some (Resize (x, y)) ->
            view_xy "resize" x y

        | Some (Visibility_change state) ->
            div [] [text "visibility change to \""; text state; text "\""]
    in
    div []
        [
            p [] [text "keyboard, mouse move/up/down, resize, visiblity change"];
            event_html;
        ]


let subscriptions (_state: state): msg Subscription.t =
    Subscription.(
        batch
            [
                on_keydown (fun key -> Got_key_down key);
                on_keyup (fun key -> Got_key_up key);
                on_mouse_down (fun x y -> Got_mouse_down (x, y));
                on_mouse_up (fun x y -> Got_mouse_up (x, y));
                on_mouse_move (fun x y -> Got_mouse_move (x, y));
                on_resize (fun x y -> Got_resize (x, y));
                on_visibility_change (fun str -> Got_visibility_change str);
            ])
