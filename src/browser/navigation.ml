type 'm key = Url.t -> 'm


type url_request = Internal of Url.t | External of string


let url_request_of_string (s: string): url_request option =
    let current = Fmlib_js.Url.current () |> Url.of_string in
    let url = Url.of_string s in
    let equal_origin url1 url2 =
        let open Url in
        url1.protocol = url2.protocol
        && url1.host = url2.host
        && url1.port = url2.port
    in
    match (current, url) with
    | (Some c, Some u) when equal_origin c u ->
        Some (Internal u)
    | (Some _, Some _) ->
        Some (External s)
    | _ ->
        None
