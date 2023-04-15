(** Http requests made in the browser.

    This module uses the javascript object [XMLHttpRequest] to make http
    requests.
*)

type t
(** Type of a http request object

   All properties of an http request object
   {{:https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest} see}.
*)


val event_target: t -> Event_target.t
(** View the request as an event target.

    It is recommended to add a listener for the [loadend] event which is fired
    after completion (successful or unsuccessful) of the request.

    The lister shall check the status property (200: Ok, 404: not found, ...).
    In case of success, the property [responseText] is a nullable string which
    contains the response.


    Status codes  {{:https://developer.mozilla.org/en-US/docs/Web/HTTP/Status}
    see}.

    The following decoder can be used to decode the status and the response text
    of a terminated request:
    {[
        Base.Decode.(
            field "target"
                (let* status   = field "status" int in
                 let* response = field "responseText" (option string) in
                 return (status, response))
        )
    ]}
*)

val make: string -> string -> (string * string) list -> string -> t
(** [make method url headers body]

*)


val ready_state: t -> int
(** Ready state of the request

    {[
        0: request not initialized
        1: open has been called
        2: send has been called
        3: loading
        4: complete
    ]}
*)


val status: t -> int
(** The status of the completed http request

    E.g.: 200: Ok, 403: forbidden, 404: not found, ...
*)


val response_text_value: t -> Base.Value.t
(** The response text of the completed http request as a javascript value. *)


val response_text_string: t -> string
(** The response text of the completed http request as a string. *)
