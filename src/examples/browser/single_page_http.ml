open Fmlib_browser

type error = Http.error

type res = (string, error) result

type state =
    {
        http_text: res option;
        http_text_sent: bool;
        http_json: res option;
        http_json_sent: bool;
    }

type msg =
    | Clicked_send_http_text
    | Clicked_send_http_json
    | Got_http_text of res
    | Got_http_json of res


let init: state * msg Command.t =
    (
        {
            http_text = None;
            http_text_sent = false;
            http_json = None;
            http_json_sent = false
        },
        Command.none
    )


let update (state: state) (msg: msg): state * msg Command.t =
    let url = "https://hbr.github.io/fmlib/webapp/data.json" in
    match msg with
    | Clicked_send_http_text ->
        let command =
            Command.attempt
                (fun res -> Got_http_text res)
                Task.(
                    let* _ = sleep 1000 () in
                    http_text "GET" url [] "")
        in
        ({state with http_text = None; http_text_sent = true}, command)
    | Clicked_send_http_json ->
        let command =
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
        ({state with http_json = None; http_json_sent = true}, command)
    | Got_http_text res ->
        ({state with http_text = Some res}, Command.none)
    | Got_http_json res ->
        ({state with http_json = Some res}, Command.none)


let view (state: state): msg Html.t =
    let open Html in
    let open Attribute in
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
        match state.http_text, state.http_text_sent with
        | None, false ->
            text "text request not sent yet"
        | None, true ->
            text "sending text request"
        | Some (Error err), _ ->
            view_error err
        | Some (Ok str), _ ->
            pre [] [text str]
    and view_http_json =
        match state.http_json, state.http_json_sent with
        | None, false ->
            text "json request not sent yet"
        | None, true ->
            text "sending json request"
        | Some (Error err), _ ->
            view_error err
        | Some (Ok str), _ ->
            text str
    in
    div []
        [
            h3 [] [ text "Text request" ];
            div [ style "margin-bottom" "1em" ] [ view_http_text ];
            button [ on_click Clicked_send_http_text ] [ text "Send" ];
            h3 [] [ text "Json request decoded" ];
            div [ style "margin-bottom" "1em" ] [ view_http_json ];
            button [ on_click Clicked_send_http_json ] [text "Send"];
        ]
