open Fmlib_browser

type state = int

type msg = Increment | Decrement


let init: state * msg Command.t =
    (0, Command.none)


let update (state: state) (msg: msg): state * msg Command.t =
    match msg with
    | Increment ->
        (state + 1, Command.none)
    | Decrement ->
        (state - 1, Command.none)


let view (state: state): msg Html.t =
    let open Html in
    let open Attribute in
    div []
        [
            button [on_click Increment] [text "^"];
            div [style "font-size" "2em"]
                [text (" " ^ string_of_int state ^ " ")];
            button [on_click Decrement] [text "v"];
        ]
