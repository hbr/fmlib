(** The content of a browser window.


    {v
    Hierarchy:

        Document                        Text node

        Element                         Character data

                        Node

                        Event Target
    v}

    I.e. a document is an element which is a node which is an event target. A text
    node is an event target as well.

    Each document has a body element which is the root of the visible document.

    Nodes can form a tree. I.e. each node has an optional parent, an optional
    first child and an optional sibling. There are methods to add and remove
    children from a node.
*)



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
    (** The optional parent of the node. *)

    val first:  t -> t option
    (** The optional first child of a node. *)

    val next:   t -> t option
    (** The optional next sibling of a node. *)

    val append: t -> t -> unit
    (** [apppend child parent] Append [child] to the end of the children of
        [parent]

        If [child] is already a child of another node, it is removed from the
        other node. A node can be a child of only one parent node.
    *)

    val remove: t -> t -> unit
    (** [remove child parent] Remove [child] from [parent]

        Precondition: [child] must be a child of [parent].

        If you are not sure that [child] belongs to [parent], get [parent child]
        and check (by physical equality [==]) that the computed parent and
        [parent] are the same.

        Procedure to remove all children from a node:
        {[
            let rec remove_children (node: t): unit =
                match parent node with
                | None ->
                    ()
                | Some child ->
                    remove child node;
                    remove_children node    (* tail recursion, compiled to a
                                               javascript loop. *)
        ]}
    *)
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




module Document:
sig
    type t
    (** Document type. *)

    val body: t -> Element.t
    (** The body of the document. *)


    val create_element:   string -> t -> Element.t
    (** [create_element tag] Create a new element with [tag]. *)

    val create_text_node: string -> t -> Node.t
    (** [create_text_node text] Create a new text node with content [text]. *)


    val create_document_fragment: t -> Node.t
    (** Create a new document fragment.

        A document fragment is a special node with no parent. Adding children to
        the fragment does not affect the dom i.e. does not cause reflow and
        repaint, because the fragment is not part of the active dom.

        If you call
        {[
            node.append(fragment)
        ]}
        all the children the [fragment] are appended to [node] instead of the
        fragment, leaving an empty fragment behind.
    *)
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
