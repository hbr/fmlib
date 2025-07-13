module type S =
sig
    type _ attr

    (** Virtual Dom *)


    type 'msg t (** Type of a virtual dom node potentially generating a message
                    of type ['msg]. *)

    val text: string -> 'msg t (** [text str] Create a text node. *)

    val node: string -> 'msg attr list -> 'msg t list -> 'msg t
    (** [node tag attrs children]

        Create an html element with a tagname, a list of attributes and a list
        of children.
    *)

    val node_ns:
        string -> string -> 'msg attr list -> 'msg t list -> 'msg t
    (** [node namespace tag attrs children]

        Like [node], but creates the node within a namespace e.g.
        "http://www.w3.org/2000/svg" for [svg] elements.
    *)



    val svg_node: string -> 'msg attr list -> 'msg t list -> 'msg t
    (** [svg_node tag attrs children]

        Create an svg element with a tagname, a list of attributes and a list
        of children. An svg element is a node in the namespace
        "http://www.w3.org/2000/svg".
    *)



    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f vdom]

        Map a virtual dom [vdom] creating messages of type ['a] to a virtual dom
        creating messages of type ['b].
    *)




    val keyed:
        string -> 'msg attr list -> (string * 'msg t) list -> 'msg t
    (** [keyed tag attrs children]

        Like [node], but add a unique identifier to each child node. This makes
        adding, removing and modifying child nodes more efficient. The dom
        diffing algorithm compares child nodes with the same identifier.
     *)



    (** {1 Headers}
     *)

    val h1: 'msg attr list -> 'msg t list -> 'msg t
    (** [h1 attrs children] is equivalent to [node "h1" attrs children]. *)

    val h2: 'msg attr list -> 'msg t list -> 'msg t
    (** [h2 attrs children] is equivalent to [node "h2" attrs children]. *)

    val h3: 'msg attr list -> 'msg t list -> 'msg t
    (** [h3 attrs children] is equivalent to [node "h3" attrs children]. *)

    val h4: 'msg attr list -> 'msg t list -> 'msg t
    (** [h4 attrs children] is equivalent to [node "h4" attrs children]. *)

    val h5: 'msg attr list -> 'msg t list -> 'msg t
    (** [h5 attrs children] is equivalent to [node "h5" attrs children]. *)

    val h6: 'msg attr list -> 'msg t list -> 'msg t
    (** [h6 attrs children] is equivalent to [node "h6" attrs children]. *)




    (** {1 Grouping Content}
     *)

    val div: 'msg attr list -> 'msg t list -> 'msg t
    (** [div attrs children] is equivalent to [node "div" attrs children]. *)

    val span: 'msg attr list -> 'msg t list -> 'msg t
    (** [span attrs children] is equivalent to [node "span" attrs children]. *)

    val pre: 'msg attr list -> 'msg t list -> 'msg t
    (** [pre attrs children] is equivalent to [node "pre" attrs children]. *)

    val p: 'msg attr list -> 'msg t list -> 'msg t
    (** [p attrs children] is equivalent to [node "p" attrs children]. *)




    (** {1 Input}
     *)

    val button: 'msg attr list -> 'msg t list -> 'msg t
    (** [button attrs children] is equivalent to [node "button" attrs children]. *)

    val input: 'msg attr list -> 'msg t list -> 'msg t
    (** [input attrs children] is equivalent to [node "input" attrs children]. *)

    val label: 'msg attr list -> 'msg t list -> 'msg t
    (** [label attrs children] is equivalent to [node "label" attrs children]. *)

    val textarea: 'msg attr list -> 'msg t list -> 'msg t
    (** [textarea attrs children] is equivalent to
        [node "textarea" attrs children]. *)

    val select: 'msg attr list -> 'msg t list -> 'msg t
    (** [select attrs children] is equivalent to [node "select" attrs children]. *)



    (** {1 Lists}
     *)

    val ol: 'msg attr list -> 'msg t list -> 'msg t
    (** [ol attrs children] is equivalent to [node "ol" attrs children]. *)

    val ul: 'msg attr list -> 'msg t list -> 'msg t
    (** [ul attrs children] is equivalent to [node "ul" attrs children]. *)

    val li: 'msg attr list -> 'msg t list -> 'msg t
    (** [li attrs children] is equivalent to [node "li" attrs children]. *)





    (** {1 Reference Nodes}

        Reference nodes are nodes whose content is not controlled by the virtual
        dom. In the virtual dom the reference nodes are inserted by their name.

        The contents of reference nodes is controlled via
        {!val:Command.set_reference}.

        Reference nodes are persistent. Once referenced by {!val:reference} or
        initialized or updated by {!val:Command.set_reference} they exist. Once
        existing they can be modidfied by {!val:Command.set_reference}.

        The virtual dom can use them or not. They remain in existence.

        Reference nodes are a means to improve performance. In the following
        examples reference nodes might be useful:

        - Having an editor window in browser (e.g. CodeMirror): It does not make
        sense and is quite difficult to control an editor window by the virtual
        dome. It is better to create a reference node and let the internal state
        of the editor handled by some other meanss (e.g. CodeMirror code)

        - Spreadsheet with many cells: In a spreadsheet usully one cell is
        updated and some cells whose content depends on the edited cell have to
        be updated as well. Having a reference node for each cell makes it
        possible to update only the edited its dependent cells. Having all
        spreadsheet cells managed by the virtual dom requires a diffing of all
        cells. This can become quite slow if the spreadsheet is large.
     *)

    val reference: string -> 'msg t
    (**
        Insert a reference element into the dom.
    *)
end
