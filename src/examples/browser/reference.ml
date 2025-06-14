open Fmlib_browser


type state = {
    main: bool;
    normal: string;
    remote: string;
    normal_value: string;
}

type msg =
    | Toggle
    | Normal of string
    | Remote of string
    | Push
    | Pull



let normal_input str = Normal str

let remote_input str = Remote str


let view_input state =
    let open Html in
    let open Attribute in
    let cell tag node str pushtag pushmsg =
        div [margin "10px"; padding "5px"]
            [ h3  [] [text tag]
               ; div [] [node]
               ; div [] [text "typed: "; text str]
               ; button [on_click pushmsg] [text pushtag]
        ]
    in
    div [ style "display" "flex"
        ; style "flex-direction" "row"
        ]
        [ cell
              "Normal"
              (
                  input [ attribute "type" "text"
                        ; placeholder "Enter text"
                        ; value state.normal_value
                        ; on_input normal_input
                        ] []
              )
              state.normal
              "Push"
              Push
        ; cell
            "Remote"
            (reference "refcel")
            state.remote
            "Push"
            Pull
        ]



let view state =
    let open Html in
    let open Attribute in
    div [] [
        button [on_click Toggle] [text "Toggle"]
        ;
        if state.main then
            div [] [text "Main Page"]
        else
            view_input state
    ]



let remote_dom str =
    let open Html in
    let open Attribute in
    input [ attribute "type" "text"
          ; placeholder "Enter text"
          ; value str
          ; on_input remote_input
          ] []



let update s = function
    | Toggle ->
        {s with main = not s.main}, Command.none

    | Normal normal ->
        {s with normal; normal_value = normal},
        Command.none

    | Remote remote ->
        {s with remote},
        Command.none

    | Push ->
        s,
        Command.set_reference "refcel" (remote_dom s.normal_value)

    | Pull ->
        {s with normal_value = s.remote},
        Command.none



let _ = basic_application
        { main = true; normal = ""; normal_value = ""; remote = ""}
        (Command.set_reference "refcel" (remote_dom ""))
        (fun s -> view s, "Reference")
        (fun _ -> Subscription.none)
        update
