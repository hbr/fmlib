open Fmlib_browser

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


let view (state: state): msg Html.t =
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
    div []
        [
            h3 [] [text "Text request"];
            view_http_text;
            h3 [] [text "Json request decoded"];
            view_http_json;
        ]
