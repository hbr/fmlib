(** Browser events. *)


type t
(** Type type of an event. *)

val value: t -> Base.Value.t
(** The event viewed as a javascript value. *)


val stop_propagation: t -> unit
(** Stop the event from bubbling up the dom tree. *)

val prevent_default:  t -> unit
(** Don't do the browser's default action associated with the event.

    E.g. the default action for a mouse click on an anchor [<a href="..">
    ... </a>] is to load the page given in [href]. The call [prevent_default
    event] inhibits this default behaviour.
*)



val mouse: string -> Base.Value.t -> t
(** [mouse typ optional]

    Create a mouse event. The type can be one of [click, dblclick, mouseup,
    mousedown, mouseenter, mouseleave, mouseout, mouseover].

    The optional is a javscript object with the following optional properties
    [screenX, screenY, ...]. The optional can be empty.
*)
