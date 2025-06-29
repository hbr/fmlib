open Fmlib_browser

(* STATE AND MESSAGES *)

type state = {
  file: File.t option;
  error: string option
}

type msg =
  | Clicked_select_file
  | Selected_file of File.t

let init: (state * msg Command.t) Decoder.t =
  Decoder.(return ({ file = None; error = None }, Command.none))

(* UPDATE *)

let update (state: state) (msg: msg): state * msg Command.t =
  match msg with
  | Clicked_select_file ->
    let cmd =
      Task.select_file
        ["image/png"; "image/jpeg"]
        (fun file -> Selected_file file)
      |> Command.perform
    in
    (state, cmd)

  | Selected_file file ->
    ({state with file = Some file}, Command.none)

(* VIEW*)

let view (state: state): msg Html.t * string =
  let open Html in
  let open Attribute in
  let title = "Select file" in
  let html =
    div []
      [
        h1 [] [text "Please select a file"];
        (match state.file with
        | None ->
            button [on_click Clicked_select_file] [text "Select file"]
        | Some file ->
            let file_info =
              Printf.sprintf
                "You selected file \"%s\", media type: %s, size: %i bytes"
                (File.name file)
                (File.media_type file |> Option.value ~default:"unknown")
                (File.size file)
            in
            p [] [ text file_info ])
      ]
  in
  (html, title)

(* SUBSCRIPTIONS *)

let subscriptions (_state: state): msg Subscription.t =
  Subscription.none

(* RUN *)

let _ = application "select_file" init view subscriptions update
