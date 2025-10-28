open Fmlib_browser

type state =
    {
        is_rolling: bool;
        die_face: string;
    }


type msg = Roll | Got_die_face of string


let init: state * msg Command.t =
    ({is_rolling = false; die_face = "?"}, Command.none)


let update (state: state) (msg: msg): state * msg Command.t =
    match msg with
    | Roll ->
        let command =
            let faces = ["⚀"; "⚁"; "⚂"; "⚃"; "⚄"; "⚅"] in
            let rand = Random.choose faces
            in
            let task = Task.(
                let* _    = sleep 1000 () in
                let* face = random rand in
                return (Got_die_face face))
            in
            Command.perform task
        in
        ({state with is_rolling = true}, command)
    | Got_die_face die_face ->
        ({is_rolling = false; die_face}, Command.none)

let view (state: state): msg Html.t =
    let open Html in
    let open Attribute in
    let face =
        if state.is_rolling then
            "🎲"
        else
            state.die_face
    in
    let attrs =
        if state.is_rolling then
            [
                font_size "2.5em";
                style "position" "relative";
                style "animation-name" "rolling";
                style "animation-duration" "0.1s";
                style "animation-iteration-count" "infinite";
            ]
        else
            [style "font-size" "3em"]
    in
    div []
        [
            div attrs [text face];
            button [title "roll the die"; on_click Roll] [text "Roll"];
        ]
