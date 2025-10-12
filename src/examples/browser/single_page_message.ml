open Fmlib_browser

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


let view (state: state): msg Html.t =
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
    div []
        [
            h3 [] [text state];
            button [on_click (Send value)] [text "send"];
        ]


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
