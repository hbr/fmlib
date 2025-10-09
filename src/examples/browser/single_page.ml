open Fmlib_browser


(* Routes
 * ======================================================================
 *)

module Route =
struct

    type t =
        | Not_found
        | Home
        | Counter
        | Digital_clock
        | Rolling_die
        | Send_message
        | Global_events
        | Input
        | Http_request


    let of_url (url: Url.t): t =
        let parser =
            let open Url.Parser in
            s "fmlib"
            </> s "webapp"
            </> one_of
                [
                    map Home (s "single_page.html");
                    map Home top;
                    map Counter (s "counter");
                    map Digital_clock (s "digital_clock");
                    map Rolling_die (s "rolling_die");
                    map Send_message (s "send_message");
                    map Global_events (s "global_events");
                    map Input (s "input");
                    map Http_request (s "http_request");
                ]
        in
        url
        |> Url.Parser.parse parser
        |> Option.value ~default:Not_found


    let href (route: t): 'msg Attribute.t =
            (match route with
             | Not_found ->
                 "not_found"
             | Home ->
                 "single_page.html"
             | Counter ->
                 "counter"
             | Digital_clock ->
                 "digital_clock"
             | Rolling_die ->
                 "rolling_die"
             | Send_message ->
                 "send_message"
             | Global_events ->
                 "global_events"
             | Input ->
                 "input"
             | Http_request ->
                 "http_request")
        |> Attribute.href

end




(* Pages
 * ======================================================================
 *)

module Not_found_page =
struct

    let view (): 'msg Html.t * string =
        let open Html in
        let html = div [] [text "Not found"] in
        (html, "Page not found")

end


module Home_page =
struct

    let view (): 'msg Html.t * string =
        let open Html in
        let html =
            ul
                []
                [
                    li [] [a [Route.href Counter] [text "Counter"]];
                    li [] [a [Route.href Digital_clock] [text "Digital clock"]];
                    li [] [a [Route.href Rolling_die] [text "Rolling die"]];
                    li [] [a [Route.href Send_message] [text "Send message to Javascript"]];
                    li [] [a [Route.href Global_events] [text "Global events"]];
                    li [] [a [Route.href Input] [text "Input"]];
                    li [] [a [Route.href Http_request] [text "Http request"]];
                ]
        in
        (html, "Home")

end


module Counter_page =
struct

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

    let view (state: state): msg Html.t * string =
        let open Html in
        let open Attribute in
        let html =
            div []
                [
                    button [on_click Increment] [text "^"];
                    div [style "font-size" "2em"]
                        [text (" " ^ string_of_int state ^ " ")];
                    button [on_click Decrement] [text "v"];
                ]
        in
        (html, "Counter")

end


module Digital_clock_page =
struct

    type state =
        {
            time: Time.t;
            zone: Time.Zone.t;
        }

    type msg = Got_time of Time.t | Got_time_zone of Time.Zone.t

    let init: state * msg Command.t =
        let command =
            let open Command in
            batch
                [
                    now (fun t -> Got_time t);
                    time_zone (fun z -> Got_time_zone z)
                ]
        in
        ({time = Time.zero; zone = Time.Zone.utc}, command)

    let update (state: state) (msg: msg): state * msg Command.t =
        match msg with
        | Got_time time ->
            ({state with time}, Command.none)
        | Got_time_zone zone ->
            ({state with zone}, Command.none)

    let subscriptions (_state: state): msg Subscription.t =
        Subscription.every 1000 (fun t -> Got_time t)

    let view (state: state): msg Html.t * string =
        let open Html in
        let open Attribute in
        let time tag time zone =
            div [margin "10px"; border_style "solid"; padding "5px"]
                [
                    div [] [text tag];
                    div []
                        [
                            text
                                (
                                    Printf.sprintf
                                        "%02d:%02d:%02d"
                                        (Time.hour   time zone)
                                        (Time.minute time zone)
                                        (Time.second time zone)
                                )
                        ];
                ]
        in
        let html =
            div
                [
                    style "font-size" "20px";
                    style "display" "flex";
                    style "flex-direction" "row";
                ]
                [
                    time "your zone" state.time state.zone;
                    time "utc" state.time Time.Zone.utc;
                ]
        in
        (html, "Digital clock")

end


module Rolling_die_page =
struct

    type state =
        {
            is_rolling: bool;
            die_face: string;
        }

    type msg = Roll | Got_die_face of string

    let roll_command =
        let faces = ["⚀"; "⚁"; "⚂"; "⚃"; "⚄"; "⚅"] in
        let rand = Random.choose faces
        in
        let task = Task.(
            let* _    = sleep 1000 () in
            let* face = random rand in
            return (Got_die_face face))
        in
        Command.perform task

    let init: state * msg Command.t =
        ({is_rolling = true; die_face = "?"}, roll_command)

    let update (state: state) (msg: msg): state * msg Command.t =
        match msg with
        | Roll ->
            ({state with is_rolling = true}, roll_command)
        | Got_die_face die_face ->
            ({is_rolling = false; die_face}, Command.none)

    let view (state: state): msg Html.t * string =
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
        let html =
            div []
                [
                    div attrs [text face];
                    button [title "roll the die"; on_click Roll] [text "Roll"];
                ]
        in
        (html, "Rolling die")

end


module Send_message_page =
struct

    type state = string

    type msg = Send of Value.t | Receive of string

    let init: state * msg Command.t =
        ("?", Command.none)

    let update (_state: state) (msg: msg): state * msg Command.t =
        match msg with
        | Send value ->
            let cmd =
                Command.just_do
                     Task.(
                         let* _ = log_string "sending to javascript ... " in
                         let* _ = log_value value >>= sleep 1000 in
                         send_to_javascript value)
            in
            ("sending", cmd)
        | Receive reply ->
            (reply, Command.none)

    let view (state: state): msg Html.t * string =
        let open Html in
        let open Attribute in
        let value =
            let open Value in
            record
                [|
                    ("first_name", string "John");
                    ("second_name", string "Doe");
                    ("age", int 45);
                |]
        in
        let html =
            div []
                [
                    h3 [] [text state];
                    button [on_click (Send value)] [text "send"];
                ]
        in
        (html, "Send message to Javascript")

    let subscriptions (_state: state): msg Subscription.t =
        let decoder =
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
                return (Receive res))
        in
        Subscription.on_message decoder

end


module Global_events_page =
struct

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

    let view (state: state): msg Html.t * string =
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
        let html =
            div []
                [
                    p [] [text "keyboard, mouse move/up/down, resize, visiblity change"];
                    event_html;
                ]
        in
        (html, "Global events")

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

end


module Input_page =
struct

    type state =
        {
            password1: string;
            password2: string;
            text_input: string;
            username: string;
            slider_value: string;
        }

    type msg =
        | Got_password1 of string
        | Got_password2 of string
        | Got_text_input of string
        | Got_username of string
        | Got_slider_value of string

    let init: state * msg Command.t =
        let state =
            {
                password1 = "";
                password2 = "";
                text_input = "";
                username = "";
                slider_value = "";
            }
        in
        (state, Command.none)

    let update (state: state) (msg: msg): state * msg Command.t =
        let state =
            match msg with
            | Got_password1 password1 ->
                {state with password1}
            | Got_password2 password2 ->
                {state with password2}
            | Got_text_input text_input ->
                {state with text_input}
            | Got_username username ->
                {state with username}
            | Got_slider_value slider_value ->
                {state with slider_value}
        in
        (state, Command.none)

    let view (state: state): msg Html.t * string =
        let open Fmlib_std in
        let open Html in
        let open Attribute in
        let password_ok =
            state.password1 = state.password2
        in
        let html =
            div []
                [
                    h3 [] [text "Text input"];
                    input
                        [
                            type_ "text";
                            placeholder "Text to reverse";
                            value state.text_input;
                            on_input (fun i -> Got_text_input i);
                        ] [];
                    p []
                        [
                            text "reversed text: ";
                            text (String.reverse state.text_input);
                        ];
                    h3 [] [text "Slider"];
                    input
                        [
                            type_ "range";
                            min "0";
                            max "10";
                            step 0.5;
                            value state.slider_value;
                            on_input (fun i -> Got_slider_value i);
                        ] [];
                    p [] [text "slider value: "; text state.slider_value];
                    h3 [] [text "Password check"];
                    input
                        [
                            type_ "text";
                            placeholder "user name";
                            value state.username;
                            on_input (fun i -> Got_username i);
                        ] [];
                    input
                        [
                            type_ "password";
                            placeholder "password";
                            value state.password1;
                            on_input (fun i -> Got_password1 i);
                        ] [];
                    input
                        [
                            type_ "password";
                            placeholder "re enter password";
                            value state.password2;
                            on_input (fun i -> Got_password2 i);
                        ] [];
                    p
                        (if password_ok then [color "green"] else [color "red"])
                        (
                            if password_ok then
                                [text "Ok"]
                            else
                                [text "passwords don't match"]
                        );
                    ]
        in
        (html, "Input")

end


module Http_request_page =
struct

    type error = Http.error

    type res = (string, error) result

    type state =
        {
            http_text: res option;
            http_json: res option;
        }

    type msg = Got_http_text of res | Got_http_json of res

    let init: state * msg Command.t =
        let url = "https://hbr.github.io/fmlib/webapp/data.json" in
        let cmd1 =
            Command.attempt
                (fun res -> Got_http_text res)
                Task.(
                    let* _ = sleep 1000 () in
                    http_text "GET" url [] "")
        in
        let cmd2 =
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
                (fun res -> Got_http_json res)
                Task.(
                    let* _ = sleep 2000 () in
                    http_json "GET" url [] None decode)
        in
        ({http_text = None; http_json = None}, Command.batch [cmd1; cmd2])

    let update (state: state) (msg: msg): state * msg Command.t =
        let state =
            match msg with
            | Got_http_text res ->
                {state with http_text = Some res}
            | Got_http_json res ->
                {state with http_json = Some res}
        in
        (state, Command.none)

    let view (state: state): msg Html.t * string =
        let open Html in
        let view_error err =
            match err with
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
        let html =
            div []
                [
                    h3 [] [text "Text request"];
                    view_http_text;
                    h3 [] [text "Json request decoded"];
                    view_http_json;
                ]
        in
        (html, "HTTP request")

end




(* State and messages
 * ============================================================
 *)

type msg =
    | Clicked_link of Navigation.url_request
    | Changed_url of Url.t
    | Got_counter_msg of Counter_page.msg
    | Got_digital_clock_msg of Digital_clock_page.msg
    | Got_rolling_die_msg of Rolling_die_page.msg
    | Got_send_message_msg of Send_message_page.msg
    | Got_global_events_msg of Global_events_page.msg
    | Got_input_msg of Input_page.msg
    | Got_http_request_msg of Http_request_page.msg

type page =
    | Not_found
    | Home
    | Counter of Counter_page.state
    | Digital_clock of Digital_clock_page.state
    | Rolling_die of Rolling_die_page.state
    | Send_message of Send_message_page.state
    | Global_events of Global_events_page.state
    | Input of Input_page.state
    | Http_request of Http_request_page.state

type state =
    {
        key: msg Navigation.key;
        page: page;
    }




(* Init function
 * ======================================================================
 *)

let change_page_to (url: Url.t) (key: msg Navigation.key): state * msg Command.t =
    let map_state_and_msg to_page to_msg (page_state, page_cmd) =
        ({page = to_page page_state; key}, Command.map to_msg page_cmd)
    in
    match Route.of_url url with
    | Not_found ->
        ({page = Not_found; key}, Command.none)
    | Home ->
        ({page = Home; key}, Command.none)
    | Counter ->
        Counter_page.init
        |> map_state_and_msg
            (fun s -> Counter s)
            (fun m -> Got_counter_msg m)
    | Digital_clock ->
        Digital_clock_page.init
        |> map_state_and_msg
            (fun s -> Digital_clock s)
            (fun m -> Got_digital_clock_msg m)
    | Rolling_die ->
        Rolling_die_page.init
        |> map_state_and_msg
            (fun s -> Rolling_die s)
            (fun m -> Got_rolling_die_msg m)
    | Send_message ->
        Send_message_page.init
        |> map_state_and_msg
            (fun s -> Send_message s)
            (fun m -> Got_send_message_msg m)
    | Global_events ->
        Global_events_page.init
        |> map_state_and_msg
            (fun s -> Global_events s)
            (fun m -> Got_global_events_msg m)
    | Input ->
        Input_page.init
        |> map_state_and_msg
            (fun s -> Input s)
            (fun m -> Got_input_msg m)
    | Http_request ->
        Http_request_page.init
        |> map_state_and_msg
            (fun s -> Http_request s)
            (fun m -> Got_http_request_msg m)


let init (url: Url.t) (key: msg Navigation.key): (state * msg Command.t) Decoder.t =
    Decoder.(return (change_page_to url key))




(* Update function
 * ======================================================================
 *)

let update (state: state) (msg: msg): state * msg Command.t =
    let map_state_and_msg to_page to_msg (page_state, page_cmd) =
        ({state with page = to_page page_state}, Command.map to_msg page_cmd)
    in
    match (msg, state.page) with
    | (Clicked_link req, _) ->
        let cmd =
            match req with
            | External e ->
                Command.load e
            | Internal i ->
                Command.push_url state.key (Url.to_string i)
        in
        (state, cmd)
    | (Changed_url url, _) ->
        change_page_to url state.key
    | (Got_counter_msg page_msg, Counter page_state) ->
        Counter_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Counter s)
            (fun m -> Got_counter_msg m)
    | (Got_digital_clock_msg page_msg, Digital_clock page_state) ->
        Digital_clock_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Digital_clock s)
            (fun m -> Got_digital_clock_msg m)
    | (Got_rolling_die_msg page_msg, Rolling_die page_state) ->
        Rolling_die_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Rolling_die s)
            (fun m -> Got_rolling_die_msg m)
    | (Got_send_message_msg page_msg, Send_message page_state) ->
        Send_message_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Send_message s)
            (fun m -> Got_send_message_msg m)
    | (Got_global_events_msg page_msg, Global_events page_state) ->
        Global_events_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Global_events s)
            (fun m -> Got_global_events_msg m)
    | (Got_input_msg page_msg, Input page_state) ->
        Input_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Input s)
            (fun m -> Got_input_msg m)
    | (Got_http_request_msg page_msg, Http_request page_state) ->
        Http_request_page.update page_state page_msg
        |> map_state_and_msg
            (fun s -> Http_request s)
            (fun m -> Got_http_request_msg m)
    | (_, _) ->
        (* Disregard messages that arrived for the wrong page *)
        (state, Command.none)




(* View functions
 * ======================================================================
 *)

let view (state: state): msg Html.t * string =
    let open Html in
    let open Attribute in
    let map_msg to_msg (html, title) = (Html.map to_msg html, title) in
    let (page_html, title) =
        match state.page with
        | Not_found ->
            Not_found_page.view ()
        | Home ->
            Home_page.view ()
        | Counter page_state ->
            Counter_page.view page_state
            |> map_msg (fun m -> Got_counter_msg m)
        | Digital_clock page_state ->
            Digital_clock_page.view page_state
            |> map_msg (fun m -> Got_digital_clock_msg m)
        | Rolling_die page_state ->
            Rolling_die_page.view page_state
            |> map_msg (fun m -> Got_rolling_die_msg m)
        | Send_message page_state ->
            Send_message_page.view page_state
            |> map_msg (fun m -> Got_send_message_msg m)
        | Global_events page_state ->
            Global_events_page.view page_state
            |> map_msg (fun m -> Got_global_events_msg m)
        | Input page_state ->
            Input_page.view page_state
            |> map_msg (fun m -> Got_input_msg m)
        | Http_request page_state ->
            Http_request_page.view page_state
            |> map_msg (fun m -> Got_http_request_msg m)
    in
    let html =
        div []
            [
                nav [margin "20px"]
                    [
                        a [Route.href Home] [text "Home"];
                        a
                            [
                                href "https://hbr.github.io/fmlib/odoc/fmlib_browser/index.html";
                                style "float" "right"
                            ]
                            [
                                text "documentation"
                            ];
                    ];
                div
                    [
                        margin "20px";
                        border_style "solid";
                        padding "0px 20px 20px 20px"
                    ]
                    [
                        h2 [] [text title];
                        page_html;
                    ];
            ]
    in
    (html, title)




(* Subscriptions function
 * ======================================================================
 *)

let subscriptions (state: state): msg Subscription.t =
    let map_msg to_msg subs = Subscription.map to_msg subs in
    match state.page with
    | Not_found ->
        Subscription.none
    | Home ->
        Subscription.none
    | Counter _ ->
        Subscription.none
        |> map_msg (fun m -> Got_counter_msg m)
    | Digital_clock page_state ->
        Digital_clock_page.subscriptions page_state
        |> map_msg (fun m -> Got_digital_clock_msg m)
    | Rolling_die _ ->
        Subscription.none
    | Send_message page_state ->
        Send_message_page.subscriptions page_state
        |> map_msg (fun m -> Got_send_message_msg m)
    | Global_events page_state ->
        Global_events_page.subscriptions page_state
        |> map_msg (fun m -> Got_global_events_msg m)
    | Input _ ->
        Subscription.none
    | Http_request _ ->
        Subscription.none




(* Application
 * ======================================================================
 *)

let _ =
    application
        "single_page_app"
        init
        view
        subscriptions
        update
        (fun req -> Clicked_link req)
        (fun url -> Changed_url url)
