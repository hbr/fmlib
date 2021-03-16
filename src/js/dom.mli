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

    {e Note:} Use the code only within a browser window. Never in node and never
    in a worker thread.
*)



module Event:
sig
    type t

    val value: t -> Base.Value.t
    val stop_propagation: t -> unit
    val prevent_default:  t -> unit
end





(** A module to add and remove event listeners from an event target.

    The following modules are event targets:

    - Node
    - Element
    - Document
    - Window
*)
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







(** Inline style of a document element. *)
module Style:
sig
    type t
    val set: string -> string -> t -> unit
    (** [set name value style] Set the property [name] to [value] in [style].

        Examples:
        {[
            set "background-color" "red"      style
            set "border"           "10px red" style
        ]}
    *)

    val remove: string -> t -> unit
    (** [remove name style] Remove the property [name] from [style]. *)
end






(** Element node. *)
module Element:
sig
    type t
    (** Type of an element node. *)

    val node:  t -> Node.t
    (** Element viewed as a node. All elements are nodes, therefore no
        conversion is necessary. *)


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
    (** Style attribute of the element. *)


    val set_attribute:    string -> string -> t -> unit
    (** [set_attribute name value element] Set the attribute [name] to [value] on
        [element].

        Examples:
        {[
            set_attribute "id"    "my-node"  element
            set_attribute "class" "my-class" element
            set_attribute "href"  "https://github.com" element
            set_attribute "type"  "range"    element
            set_attribute "type"  "password" element
            set_attribute "placeholder" "sample-text" element
        ]}

    *)

    val remove_attribute: string -> t -> unit
    (** [remove_attribute name element] Remove the attribute [name] from
        [element]. *)


    val set_property:     string -> Base.Value.t -> t -> unit
    (** [set_property name value element] Set the property [name] to [value] in
        [element].

        The distinction between attributes and properties is subtle. Attribute
        have always a string value and implicitely set the corresponding
        property as well. A property can have any javascript value and does not
        set the corresponding attribute, even if the property has a string
        value.

        Examples:
        {[
            set_property "value" "my-text" input_element
        ]}
    *)


    val delete_property:  string -> t -> unit
    (** [delete_property name element] Delete the property [name] from
        [element]. *)


    val focus: t -> unit
    (** Put the element into keyboard focus. *)


    val blur:  t -> unit
    (** Unfocus the element. *)
end




module Document:
sig
    type t
    (** Document type. *)

    val body: t -> Element.t
    (** The body of the document. *)


    val find: string -> t -> Element.t option
    (** [find id doc] Find the element with the id attribute [id] in the
        document [doc]. Uses the javascript method [getElementById]. *)


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





(** Module representing a browser window.

    Use this module only in code executing within a browswer window. Don't use
    it in node and not in a web worker either.
*)
module Window:
sig
    type t
    val get: unit -> t
    (** Get the window object. *)

    val event_target: t -> Event_target.t
    (** The window as an event target. *)

    val document: t -> Document.t
    (** The document of the window. *)

    val on_next_animation: (float -> unit) -> t -> unit
    (** [on_next_animation callback]

        The [callback] is called on the next animation frame. Usually a browser
        has 60 frames per second.

        The argument received by the callback is the time in milliseconds since
        the current document has been loaded.
    *)
end
