open Js_of_ocaml


let percent_encode (s: string): string =
    let open Js in
    (* We can safely ignore the exception encodeURI throws for invalid
       input because Js.string always produces valid UTF-16. *)
    s |> string |> encodeURI |> to_string


let percent_decode (s: string): string option =
    let open Js in
    try s |> string |> decodeURI |> to_string |> Option.some with
    | _ -> None


let percent_encode_component (s: string): string =
    let open Js in
    (* We can safely ignore the exception encodeURIComponent throws for invalid
       input because Js.string always produces valid UTF-16. *)
    s |> string |> encodeURIComponent |> to_string


let percent_decode_component (s: string): string option =
    let open Js in
    try s |> string |> decodeURIComponent |> to_string |> Option.some with
    | _ -> None


let current (): string =
    let open Js in
    to_string Dom_html.window##.location##.href
