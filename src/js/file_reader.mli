(** The javascript file reader object *)

type t
(** The type of the file reader. *)


val event_target: t -> Event_target.t
(** View the file reader as an event target.

    For reading the contents of a file into memory, an event handler for the
    [loadend] event has to be registered and a read function, e.g. {!read_text}
    has to be called. The handler can then obtain the file contents by calling
    {!result} and decode them using one of the following decoders:

    - {!Base.Decode.string}, if {!read_text} was used
*)


val make: unit -> t
(** Create a file reader. *)


val ready_state: t -> int
(** Ready state of the read operation.

    {[
        0: empty
        1: loading
        2: done
    ]}
*)


val read_text: t -> File.t -> unit -> unit
(** Read the file contents into a string. *)


val result: t -> Base.Value.t option
(** The result of the read operation or [None] if an error occurred. *)
