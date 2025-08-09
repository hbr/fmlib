(** URL utility functions *)


val percent_encode: string -> string
(** [percent_encode s] encodes the given string according to the
    rules defined in {{:https://datatracker.ietf.org/doc/html/rfc3986} RFC 3986}.
    This is meant to be called on a full URL. Characters that are part of the
    URL syntax such as '/' or '?' are preserved.

    This is a wrapper around Javascript's {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI} encodeURI}.
*)


val percent_decode: string -> string option
(** [percent_decode s] decodes the given string according to the
    rules defined in {{:https://datatracker.ietf.org/doc/html/rfc3986} RFC 3986}.
    This is meant to be called on a full URL.

    This is a wrapper around Javascript's {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI} decodeURI}.
*)


val percent_encode_component: string -> string
(** [percent_encode_component s] encodes the given string according to the
    rules defined in {{:https://datatracker.ietf.org/doc/html/rfc3986} RFC 3986}.
    This is meant to be called on a URL component (a path, query string, fragment
    etc.). Characters that are part of the URL syntax such as '/' or '?' are
    replaced.

    This is a wrapper around Javascript's {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent} encodeURIComponent}.
*)


val percent_decode_component: string -> string option
(** [percent_decode_component s] decodes the given string according to the
    rules defined in {{:https://datatracker.ietf.org/doc/html/rfc3986} RFC 3986}.
    This is meant to be called on a URL component (a path, query string, fragment
    etc.).

    This is a wrapper around Javascript's {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent} decodeURIComponent}.
*)

val current: unit -> string
(** [current ()] returns the location of the current document
    ([Window.location]). This is always a full URL, including the scheme and
    authority parts. *)
