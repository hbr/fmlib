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






(** A node in the dom tree. *)
module Node:
sig
    type t

    val event_target: t -> Event_target.t
    (** The node viewed as an event target. *)

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
                match first node with
                | None ->
                    ()
                | Some child ->
                    remove child node;
                    remove_children node    (* tail recursion, compiled to a
                                               javascript loop. *)
        ]}
    *)

    val replace: t -> t -> t -> unit
    (** [replace new_child old_child parent]

        Put [new_child] at the position of [old_child] within [parent] which is
        the parent of [old_child].

        Precondition: [old_child] must be a child of [parent].
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



    val scroll_width: t -> int
    (** Width of the element, including content not visible due to overflow.

        It includes padding, but not the border and margins.
    *)


    val scroll_height: t -> int
    (** Height of the element, including content not visible due to overflow.
        It includes padding, but not margin and border.
    *)

    val client_width: t -> int
    (** Width of the visible part of the element with padding, without border
        and margins. *)

    val client_height: t -> int
    (** Height of the visible part of the element. It includes padding, but not
        margins, border and scrollbars. *)

    val scroll_left: t -> int
    (** Number of pixels the element's content is scrolled from its left edge.
    *)

    val scroll_top: t -> int
    (** Number of pixels the element's content is scrolled vertically.
    *)


    val set_scroll_left: int -> t -> unit
    (** [set_scroll_left pxs element] Set the left scroll position to [pxs]. *)

    val set_scroll_top:  int -> t -> unit
    (** [set_scroll_top pxs element] Set the top scroll position to [pxs]. *)



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


    val title: t -> string
    (** The title of the document. *)


    val set_title: string -> t -> unit
    (** [set_title str doc] Set the title of the document. *)


    val body: t -> Element.t
    (** The body of the document.

        Precondition: The document must have been loaded!

        Never call this function before the page has been loaded.
    *)


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





(** Module representing location.

    General structure of an url or uri:

    {v
        https://example.com:8042/over/there?name=ferret#nose
        \___/   \______________/\_________/ \_________/ \__/
          |            |            |            |        |
        scheme     authority       path        query   fragment
    v}


*)
module Location:
sig
    type t

    val href:     t -> string
    (** The full url string of the location. *)

    val protocol: t -> string
    (** The protocol i.e. either [http] or [https]. *)

    val host:     t -> string
    (** The host name i.e. the part of the authority before the port. *)

    val port:     t -> string
    (** Port number as a string. If there is no explicit port, then the empty
        string is returned. *)

    val pathname: t -> string
    (** An initial '/' followed by the path of the url of the location (or an
        empty string if there is no path). *)

    val search:   t -> string
    (** The query string. *)

    val hash:     t -> string
    (** The fragment part of the url. *)


    val assign: string -> t -> unit
    (** [assign url window] Load the [url]. *)

    val reload: t -> unit
    (** Reload the current page. *)
end





(** Module representing the browser history. *)
module History:
sig
    type t

    val go: int -> t -> unit

    val push_state: Base.Value.t -> string -> string -> t -> unit
    (** [push_state state title url] Push a new url onto the history stack
        without loading the page.

        The title is ignored by many browsers.

        The url can be relative or absolute. If its relative, it is resolved
        relative to the current url.

        Precondition: [url] must be of the same origin as the current url.

        The state is included in the corresponding popstate event as [state]
        property. The popstate event fires if the user navigates to the
        corresponding url.

        {e Note:} Neither [push_state] nor [replace_state] do trigger a popstate
        event. A popstate event is triggered only by navigation (back, forward
        button). At the popstate event the new location has already been set.
    *)


    val replace_state: Base.Value.t -> string -> string -> t -> unit
    (** Like [push_state] but without pushing a new state onto the history. Just
        replace the current entry in the history stack. *)
end




(** Module representing a browser window.

    Use this module only in code executing within a browswer window. Don't use
    it in node and not in a web worker either.

    The window object is an event target for the following events and many more
    which can propagate from inner elements.

    - load
    - resize
    - popstate
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

    val history: t -> History.t
    (** Browser history. *)


    val location: t -> Location.t
    (** Location object. *)


    val on_next_animation: (float -> unit) -> t -> unit
    (** [on_next_animation callback window]

        The [callback] is called on the next animation frame. Usually a browser
        has 60 frames per second.

        The argument received by the callback is the time in milliseconds since
        the current document has been loaded.
    *)

end
