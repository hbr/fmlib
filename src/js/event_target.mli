(** Event_targets in the browser *)


type t
(** Type of an event target. *)


val add:    string -> (Event.t -> unit) -> t -> unit
(** [add type handler target]

    Add for the event type [type] the listener [handler] to the event target
    [tgt] (javascript method [addEventListener]).
*)

val remove: string -> (Event.t -> unit) -> t -> unit
(** [remove type handler target]

    Remove for the event type [type] the listener [handler] off the event
    target [tgt] (javascript method [removeEventListener]).

    Note: The same handler might be added for several event types. The
    function [remove] only removes the handler for the specific event type.
*)

val dispatch: Event.t -> t -> bool
