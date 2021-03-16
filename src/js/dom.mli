module Style:
sig
    type t
    val set: string -> string -> t -> unit
    val remove: string -> t -> unit
end


module Event:
sig
    type t

    val value: t -> Base.Value.t
    val stop_propagation: t -> unit
    val prevent_default:  t -> unit
end



module Event_target:
sig
    type t
    val add:    string -> (Event.t -> unit) -> t -> unit
    val remove: string -> (Event.t -> unit) -> t -> unit
end





module Node:
sig
    type t

    val event_target: t -> Event_target.t

    val parent: t -> t option
    val first:  t -> t option
    val next:   t -> t option

    val append: t -> t -> unit
    (** [apppend child parent] *)

    val remove: t -> t -> unit
    (** [remove child parent] *)
end





module Element:
sig
    type t
    val node:  t -> Node.t

    val scroll_width: t -> float
    (** Minimal width needed for the element to be shown without a scrollbar.

        It includes padding, but not the border and margins.
    *)

    val client_width: t -> float
    (** Width of the element with padding, without border and margins. *)


    val scroll_left: t -> float
    (** Number of pixels the element's content is scrolled from its left edge.
    *)

    val scroll_top: t -> float
    (** Number of pixels the element's content is scrolled vertically.
    *)

    val style: t -> Style.t
    val set_attribute:    string -> string -> t -> unit
    val remove_attribute: string -> t -> unit
    val set_property:     string -> Base.Value.t -> t -> unit
    val delete_property:  string -> t -> unit
    val focus: t -> unit
    val blur:  t -> unit
end




module Text_node:
sig
    type t
    val node: t -> Node.t
end




module Document:
sig
    type t
    val body: t -> Element.t
    val create_element:   string -> t -> Element.t
    val create_text_node: string -> t -> Text_node.t
end





module Window:
sig
    type t
    val get: unit -> t
    val document: t -> Document.t
    val add_listener:    string -> (Event.t -> unit) -> t -> unit
    val remove_listener: string -> (Event.t -> unit) -> t -> unit

    val on_next_animation: (float -> unit) -> t -> unit
    (** [on_next_animation callback]

        The [callback] is called on the next animation frame. Usually a browser
        has 60 frames per second.

        The argument received by the callback is the time in milliseconds since
        the current document has been loaded.
    *)
end
