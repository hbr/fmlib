open Fmlib_js

type error = [ `Status of int | `No_json | `Decode ]


module Body =
struct
    type contents = String of string | File of File.t

    type t = {
        contents : contents;
        media_type : string option;
    }

    let empty : t =
        { contents = String ""; media_type = None }

    let string (media_type : string) (s : string) : t =
        { contents = String s; media_type = Some media_type }

    let json (v : Value.t) : t =
        (* it's ok to call Option.get here because v is constructed with one of
           the functions from Fmlib_browser.Value and thus is guaranteed to be
           serializable and its serialization is a string. *)
        let json =
            v
            |> Value.stringify
            |> Decoder.string
            |> Option.get
        in
        { contents = String json; media_type = Some "application/json" }

    let file (file: File.t): t =
        { contents = File file; media_type = File.media_type file }
end


module Expect =
struct
    type 'a t = Http_request.t -> ('a, error) result

    let string : string t =
        fun req ->
        Ok (Http_request.response_text_string req)

    let json (decode : 'a Decoder.t) : 'a t =
        fun req ->
        match Value.parse (Http_request.response_text_value req) with
        | None ->
            Error `No_json
        | Some v ->
            match decode v with
            | None ->
                Error `Decode
            | Some a ->
                Ok a

    let map (f : ('a -> 'b)) (expect : 'a t): 'b t =
        fun req ->
        Result.map f (expect req)

end
