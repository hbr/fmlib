open Fmlib_browser



module Page =
struct
    type ('s, 'm) t = {
        description: string;
        title:    string;
        view:    's -> 'm Html.t;
        subs:    'm Subscription.t;
        command: 'm Command.t
    }

    let make description title view subs command: ('s, 'm) t =
        {description; title; view; subs; command}

    let title page = page.title

    let description page = page.description

    let view page = page.view

    let subscription page = page.subs

    let command page = page.command
end


module Global_event =
struct
    type t =
        | Key_down of string
        | Key_up   of string
        | Mouse_down of int * int
        | Mouse_move of int * int
        | Mouse_up   of int * int
        | Resize     of int * int
        | Visibility_change of string
end



module Http =
struct
    type error = Task.Http.error
    type res = (string, error) result
end







(* State and messages
 * ============================================================
 *)
type state = {
    menu:    bool;
    page:    page;
    counter: int;
    time:    Time.t;
    zone:    Time.Zone.t;
    die_face:    string;
    die_rolling: bool;
    message: string;
    global_event: Global_event.t option;
    text_input: string;
    user_name: string;
    password1: string;
    password2: string;
    slider_value: string;
    http_text: Http.res option;
    http_json: Http.res option;
}

and page =
    (state, msg) Page.t

and msg =
    | Show_menu
    | Show_page
    | To_page of page
    | Decrement
    | Increment
    | Time of Time.t
    | Time_zone of Time.Zone.t
    | Roll
    | Die_face of string
    | Send of Value.t
    | Receive of string
    | Key_down of string
    | Key_up   of string
    | Mouse_down of int * int
    | Mouse_move of int * int
    | Mouse_up   of int * int
    | Resize of     int * int
    | Visibility_change of string
    | Text_input of string
    | User_name  of string
    | Password1  of string
    | Password2  of string
    | Slider_value of string
    | Http_text of Http.res
    | Http_json of Http.res


let to_page page   = To_page page
let time_msg time  = Time time
let zone_msg zone  = Time_zone zone
let die_face face  = Die_face face
let send_msg value = Send value
let receive_msg s  = Receive s
let keydown s      = Key_down s
let keyup s        = Key_up s
let mouse_down x y = Mouse_down (x, y)
let mouse_up   x y = Mouse_up (x, y)
let mouse_move x y = Mouse_move (x, y)
let resize x y     = Resize (x, y)
let visibility_change str = Visibility_change str
let text_input str = Text_input str
let user_name  str = User_name str
let password1  str = Password1 str
let password2  str = Password2 str
let slider_value i = Slider_value i
let http_text res  = Http_text res
let http_json res  = Http_json res











(* Pages
 * ======================================================================
 *)


let counter_page: page =
    let view state =
        let open Html in
        let open Attribute in
        node "div" [] [
            button [on_click Increment] [text "^"];
            div
                [style "font-size" "2em"
                ]
                [text (" " ^ string_of_int state.counter ^ " ")];
            button [on_click Decrement] [text "v"];
        ]
    in
    Page.make "Counter" "Counter" view Subscription.none Command.none





let digital_clock_page: page =
    let view state =
        let open Html in
        let open Attribute in
        let time tag zone time =
            div [margin "10px"; border_style "solid"; padding "5px"]
                [ div [] [text tag]
                ; div [] [
                      text (
                          Printf.sprintf
                              "%02d:%02d:%02d"
                              (Time.hour   zone time)
                              (Time.minute zone time)
                              (Time.second zone time))
                  ]
                ]
        in
        div [ style "font-size" "20px"
            ; style "display" "flex"
            ; style "flex-direction" "row"
            ]
            [ time "your zone" state.time state.zone
            ; time "utc" state.time Time.Zone.utc
            ]
    and subs =
        Subscription.every 1000 time_msg
    and get_time =
        Command.(perform Task.now       |> map time_msg)
    and get_zone =
        Command.(perform Task.time_zone |> map zone_msg)
    in
    let get_time_info = Command.batch [get_time; get_zone]
    in
    Page.make "Digital clock" "DClock" view subs get_time_info





let rolling_die_page: page =
    let faces = ["⚀"; "⚁"; "⚂"; "⚃"; "⚄"; "⚅"]
    in
    let view state =
        let open Html in
        let open Attribute in
        let face =
            if state.die_rolling then
                "🎲"
            else
                state.die_face
        in
        let attrs =
            if state.die_rolling then
                [font_size "2.5em"
                ; style "position" "relative"
                ; style "animation-name" "rolling"
                ; style "animation-duration" "0.1s"
                ; style "animation-iteration-count" "infinite"
                ]
            else
                [style "font-size" "3em"]
        in
        div [] [
            div attrs [text face]
          ; button [title "roll the die"; on_click Roll] [text "Roll"]
        ]
    and command =
        let rand = Random.choose faces
        in
        let task = Task.(
            let* _    = sleep 1000 () in
            let* face = random rand in
            return (Die_face face)
        )
        in
        Command.perform task
    in
    Page.make "Rolling die" "Die" view Subscription.none command




let send_message_page: page =
    let value_ =
        Value.(record [| "first_name", string "John"
                       ; "second_name", string "Doe"
                       ; "age", int 45 |])
    and decoder =
        Decoder.(
            let* fname = field "first_name" string in
            let* sname = field "second_name" string in
            let* age   = field "age" int in
            let res =
                if fname = "John" && sname = "Doe" && age = 45 then
                    "received correctly"
                else
                    "fail"
            in
            return (Receive res)
        )
    in
    let view state =
        let open Html in
        let open Attribute in
        div [] [
            h3 [] [text state.message]
          ; button
                [on_click (Send value_)]
                [text "send"]
        ]
    and sub =
        Subscription.on_message decoder
    in
    Page.make "Send message to javascript" "Send" view sub Command.none





let global_event_page: page =
    let view state =
        let open Html
        in
        let view_xy str x y =
            let xstr = string_of_int x
            and ystr = string_of_int y
            in
            div [] List.(map text [str; ": "; xstr; "/"; ystr])
        in
        let view_mouse str x y =
            view_xy ("mouse " ^ str) x y
        in
        let event_html =
            let open Global_event
            in
            match state.global_event with
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
        div [] [
            p [] [text "keyboard, mouse move/up/down, resize, visiblity change"]
          ; event_html
        ]

    and sub =
        Subscription.(batch [
            on_keydown    keydown
          ; on_keyup      keyup
          ; on_mouse_down mouse_down
          ; on_mouse_up   mouse_up
          ; on_mouse_move mouse_move
          ; on_resize resize
          ; on_visibility_change visibility_change
        ])
    in
    Page.make "Global events" "Events" view sub Command.none




let input_page: page =
    let view state =
        let open Fmlib_std in
        let open Html in
        let open Attribute in
        let password_ok =
            state.password1 = state.password2
        in
        div [] [
            h3 [] [text "Text input"]
          ; input [ attribute "type" "text"
                  ; placeholder "Text to reverse"
                  ; value state.text_input
                  ; on_input text_input
                  ] []
          ; p [] [
              text "reversed text: "
            ; text (String.reverse state.text_input)
          ]
          ; h3 [] [text "Slider"]
          ; input [ attribute "type" "range"
                  ; attribute "min" "0"
                  ; attribute "max" "10"
                  ; attribute "step" "0.5"
                  ; value state.slider_value
                  ; on_input slider_value
                  ] []
          ; p [] [text "slider value: "; text state.slider_value]
          ; h3 [] [text "Password check"]
          ; input [ attribute "type" "text"
                  ; placeholder "user name"
                  ; value state.user_name
                  ; on_input user_name
                  ] []
          ; input [ attribute "type" "password"
                  ; placeholder "password"
                  ; value state.password1
                  ; on_input password1
                  ] []
          ; input [ attribute "type" "password"
                  ; placeholder "re enter password"
                  ; value state.password2
                  ; on_input password2
                  ] []
          ; p
                (if password_ok then [color "green"]
                 else [color "red"])
                (if password_ok then
                     [text "Ok"]
                 else
                     [text "passwords don't match"])
        ]
    in
    Page.make "Input" "Input" view Subscription.none Command.none




let http_page: page =
    let url =
        "https://hbr.github.io/fmlib/webapp/data.json"
    in
    let view state =
        let open Html in
        let view_error = function
            | `Status status ->
                text ("error: http status = " ^ string_of_int status)
            | `No_json ->
                text "error: invalid json"
            | `Decode ->
                text "error: cannot decode javascript object"
        in
        let view_http_text =
            match state.http_text with
            | None ->
                text "sending text request"
            | Some (Error err) ->
                view_error err
            | Some (Ok str) ->
                pre [] [text str]
        and view_http_json =
            match state.http_json with
            | None ->
                text "sending json request"
            | Some (Error err) ->
                view_error err
            | Some (Ok str) ->
                text str
        in
        div [] [
            h3 [] [text "Text request"]
          ; view_http_text
          ; h3 [] [text "Json request decoded"]
          ; view_http_json
        ]
    and cmd1 =
        Command.attempt
            http_text
            Task.(
                let* _ = sleep 1000 () in
                Http.text "GET" url [] ""
            )
    and cmd2 =
        let decode =
            Decoder.(
                let* first_name = field "first_name" string in
                let* last_name  = field "last_name"  string in
                let* age        = field "age" int in
                return (
                    String.concat
                        " "
                        [first_name; last_name; "age"; string_of_int age]))
        in
        Command.attempt
            http_json
            Task.(
                let* _ = sleep 2000 () in
                Http.json "GET" url [] None decode
            )
    in
    Page.make
        "Http request"
        "Http"
        view
        Subscription.none
        (Command.batch [cmd1; cmd2])











(* View functions
 * ======================================================================
 *)

let view_menu (_: state): msg Html.t =
    let open Html in
    let open Attribute in
    ul
        [margin "20px"]
        (List.map
             (fun page ->
                  li
                      [on_click (To_page page)]
                      [button [] [text (Page.description page)]]
             )
             [ counter_page
             ; digital_clock_page
             ; rolling_die_page
             ; send_message_page
             ; global_event_page
             ; input_page
             ; http_page])





let view_page (state: state): 'msg Html.t =
    let open Html in
    let open Attribute in
    div [margin "20px"; border_style "solid"; padding "0px 20px 20px 20px"]
        [ h2 [] [text (Page.description state.page)]
        ; Page.view state.page state
        ]



let view (state: state): msg Html.t * string =
    let open Html in
    let open Attribute in
    let menu_button =
        if state.menu then
            button [on_click Show_page] [text "Show page"]
        else
            button [on_click Show_menu] [text "Show menu"]
    in
    let html =
        div [] [
            menu_button
          ; div [] [
                if state.menu then
                    view_menu state
                else
                    view_page state
            ]
        ]
    and title =
        if state.menu then
            "Webapp"
        else
            Page.title state.page
    in
    html, title










(* Init function
 * ======================================================================
 *)


let init: (state * msg Command.t) Decoder.t =
    let start_page = counter_page in
    Decoder.(return (
        {
            menu    = true;
            page    = start_page;
            counter = 0;
            time    = Time.zero;
            zone    = Time.Zone.utc;
            die_face     = "?";
            die_rolling  = true;
            message      = "?";
            global_event = None;
            text_input   = "";
            user_name    = "";
            password1    = "";
            password2    = "";
            slider_value = "0";
            http_text = None;
            http_json = None;
        }
        ,
        (Page.command start_page)))








(* Update function
 * ======================================================================
 *)


let update (state: state): msg -> state * msg Command.t = function
    | Show_page ->
        {state with menu = false},
        Command.none

    | Show_menu ->
        {state with menu = true},
        Command.none

    | To_page page ->
        {state with
         menu = false;
         page;
         die_rolling = true;
         global_event = None;
         http_text = None;
         http_json = None;
        },
        Page.command page

    | Decrement ->
        {state with counter = state.counter - 1},
        Command.none

    | Increment ->
        {state with counter = state.counter + 1},
        Command.none

    | Time time ->
        {state with time},
        Command.none

    | Time_zone zone ->
        {state with zone},
        Command.none

    | Roll ->
        {state with die_rolling = true},
        Page.command state.page

    | Die_face die_face ->
        {state with die_face; die_rolling = false},
        Command.none

    | Send value ->
        {state with message = "sending"},
        (Command.just_do
             Task.(
                 let* _ = log_string "sending to javascript ... " in
                 let* _ = log_value value >>= sleep 1000 in
                 send_to_javascript value
             ))

    | Receive message ->
        {state with message},
        Command.none

    | Key_down key ->
        {state with global_event = Some (Global_event.Key_down key)},
        Command.none

    | Key_up key ->
        {state with global_event = Some (Global_event.Key_up key)},
        Command.none

    | Mouse_down (x, y) ->
        {state with global_event = Some (Global_event.Mouse_down (x, y))},
        Command.none

    | Mouse_move (x, y) ->
        {state with global_event = Some (Global_event.Mouse_move (x, y))},
        Command.none

    | Mouse_up (x, y) ->
        {state with global_event = Some (Global_event.Mouse_up (x, y))},
        Command.none

    | Resize (x, y) ->
        {state with global_event = Some (Global_event.Resize (x, y))},
        Command.none

    | Visibility_change str ->
        {state with global_event = Some (Global_event.Visibility_change str)},
        Command.none

    | Text_input text_input ->
        {state with text_input}
        , Command.none

    | User_name user_name ->
        {state with user_name}
        , Command.none

    | Password1 password1 ->
        {state with password1}
        , Command.none

    | Password2 password2 ->
        {state with password2}
        , Command.none

    | Slider_value slider_value ->
        {state with slider_value}
        , Command.none

    | Http_text res ->
        {state with http_text = Some res},
        Command.none

    | Http_json res ->
        {state with http_json = Some res},
        Command.none




let subscriptions (state: state): msg Subscription.t =
    Page.subscription state.page



let _ =
    application "webapp" init view subscriptions update
