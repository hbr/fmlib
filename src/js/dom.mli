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
end



module Document:
sig
    type t
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
end
