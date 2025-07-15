module type S =
sig
    (** Data of an Http Request. *)

    type file
    type value
    type _ decoder


    type error = [
        | `Status of int
        (**
           - 0: no internet, server not found, timeout, ...
           - 401: bad request
           - 403: forbidden
           - 404: page not found
           - ...

        *)
        | `No_json (** Resource is not a valid json file *)
        | `Decode (** Resource is a valid json file, but the decoder could not
                      decode the corresponding javascript object. *)
    ]


    module Body:
    sig
        (** Body of an Http Request *)

        type t

        val empty : t
        (** The body will be empty. *)

        val string : string -> string -> t
        (** [string media_type s]

            The body will be the string [s]. The [Content-Type] header
            will be automatically set to the given [media_type]. For common
            media types, a.k.a. MIME types, see
            {{: https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types/Common_types } this list}. *)

        val json : value -> t
        (** [json v]

            The body will be [v], encoded as json. The [Content-Type] header
            will be automatically set to [application/json]. *)

        val file : file -> t
        (** [file f]
            The body will be the contents of file [f]. If the media type of [f]
            can be determined using {!File.media_type}, the [Content-Type]
            header will be automatically set to that media type. *)

    end


    module Expect:
    sig
        (** Expected Response of an Http Request *)

        type 'a t

        val string : string t
        (** The response is expected to be a string and will not be decoded
            further. *)

        val json : 'a decoder -> 'a t
        (** [json decoder]

            The response is expected to be json and will be decoded with
            [decoder]. *)

        val map : ('a -> 'b) -> 'a t -> 'b t
        (** [map f expect]

            Map the result of [expect] via the function [f] to produce a
            message. This is meant to be used in combination with
            {!Command.http_request}. *)
    end

end
