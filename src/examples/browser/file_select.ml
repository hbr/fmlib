open Fmlib_browser


(* STATE AND MESSAGES *)

type state = {
    file: File.t option;
    file_contents: (string option, string) result;
}


type msg =
    | Clicked_select_file
    | Selected_file of File.t
    | Got_file_contents of string
    | Got_error of string


let init: state =
    { file = None; file_contents = Ok None }




(* UPDATE *)

let update (state: state) (msg: msg): state * msg Command.t =
    match msg with
    | Clicked_select_file ->
        let cmd =
            Command.select_file ["text/plain"] (fun file -> Selected_file file)
        in
        (state, cmd)
    | Selected_file file ->
        let cmd = Command.file_text file (fun t ->
            match t with
            | Ok t ->
                Got_file_contents t
            | Error _ ->
                Got_error "Failed to read file")
        in
        ({ state with file = Some file }, cmd)

    | Got_file_contents contents ->
        ({ state with file_contents = Ok (Some contents) }, Command.none)

    | Got_error err ->
        ({ state with file_contents = Error err }, Command.none)




(* VIEW*)

let view_file_info (file: File.t): msg Html.t =
    let open Html in
    let open Attribute in
    p [ color "green" ] [
        text (
            Printf.sprintf
                "You selected file \"%s\", media type: %s, size: %i bytes"
                (File.name file)
                (File.media_type file |> Option.value ~default:"unknown")
                (File.size file)
        )
    ]


let view (state: state): msg Html.t * string =
    let open Html in
    let open Attribute in
    let title = "Select file" in
    let html =
        div []
            [
                h1 [] [text "File selection"];
                button [on_click Clicked_select_file] [ text "Select file" ];
                h2 [] [text "File info"];
                (
                    match state.file with
                    | None ->
                        p [ color "grey" ]
                            [ text "File info will be displayed here"]
                    | Some file ->
                        view_file_info file
                );
                h2 [] [text "File contents"];
                (
                    match state.file_contents with
                    | Error err ->
                        p [ color "red" ] [ text err ]
                    | Ok None ->
                        p [ color "grey" ]
                            [ text "File contents will be displayed here" ]
                    | Ok (Some s) ->
                        pre [] [ text s ]
                )
            ]
    in
    (html, title)




(* SUBSCRIPTIONS *)

let subscriptions (_state: state): msg Subscription.t =
    Subscription.none




(* RUN *)

let _ =
    basic_application init Command.none view subscriptions update
