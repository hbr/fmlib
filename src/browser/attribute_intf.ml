module type S =
sig
    type _ decoder
    type value
    type stop
    type prevent
    type file

    (** Attributes of Dom Elements.

        There are four types of attributes:

        - style attributes: Color, font size, etc.

        - property attributes: arbitrary javascript values as properties of the
        corresponding dom element.

        - attributes: string valued attributes to give the element an id, a
        classname, etc.

        - handler attributes: React to user interactions (mouse clicks, ...) on
        the html element.

        Sometimes the distinction between properties and attributes is quite
        subtle.  To the best of my knowledge each added attribute adds a
        property with the same name (except when the name is a javascript
        keyword like "class") to the html element. But not all properties even
        if it is a string value adds an attribute to the element.

     *)


    (** {1 Generic Interface}
     *)

    type 'msg t (** Type of an attribute potentially generating a message of
                    type ['msg]. *)


    val style: string -> string -> 'msg t
    (** [style key value] Add a style attribute to the html element.

        Examples:
        {[
            style "color"  "red"
            style "margin" "20px"
        ]}
     *)

    val property: string -> value -> 'msg t
    (** [property key value] Add a javascript property to the html element. *)


    val attribute: string -> string -> 'msg t
    (** [attribute key value] Add an attribute to the html element.

        Examples:
        {[
            attribute "id" "my_element"
            attribute "class" "container"
        ]}
    *)


    val handler:
        string
        -> stop
        -> prevent
        -> 'msg decoder
        -> 'msg t
    (** [handler event_type stop_flag prevent_flag decoder]

        Attribute representing an event listener on an html element. The two
        flags decide if the event is propagated upwards in the dom tree and if
        default action (some events like clicking on an anchor element cause
        default actions in the browser) is prevented.

        The decoder decodes the javascript event object into a message of type
        ['msg].

        Starting from the event object information from the whole dom tree up to
        the root can be decode. Each event object has a target (which is the
        element on which it is fired). The target element has a tag name, can
        have various properties etc. For more details on event objects see the
        {{: https://developer.mozilla.org/en-US/docs/Web/API/Event} event api}.

        More information on {{!page-doc_event_handler} event handlers}.
     *)


    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f a] Map an attribute creating messages of type ['a] to an
        attribute creating messages of type ['b].
    *)


    (** {1 Handler} *)

    val on: string -> 'msg decoder -> 'msg t
    (** [on event_type decoder]

        is equivalent to
        [handler event_type Event_flag.no_stop Event_flag.no_prevent decoder]
    *)

    val on_click: 'msg -> 'msg t
    (** [on_click m] produce the message [m] on mouse click. *)


    val on_keydown: (string -> 'msg) -> 'msg t
    (** [on_keydown f]

        Produce the message [f key] on the keydown event with [key].
    *)

    val on_keyup: (string -> 'msg) -> 'msg t
    (** [on_keyup f]

        Produce the message [f key] on the keyup event with [key].
    *)






    (** {1 Common style attributes} *)

    val font_size: string -> 'msg t
    (** Example [font_size "20px"]

        Abbreviates [style "font-size" "20px"].
    *)


    val color: string -> 'msg t
    (** Example [color "red"]

        Abbreviates [style "color" "red"].
    *)


    val background_color: string -> 'msg t
    (** Example [background_color "powderblue"]

        Abbreviates [style "background-color" "powderblue"].
    *)


    val height: string -> 'msg t
    (** Example [height "200px"]

        Abbreviates [style "height" "200px"].
    *)


    val width: string -> 'msg t
    (** Example [width "200px"]

        Abbreviates [style "width" "200px"].
    *)



    (** {1 Margin, border, padding and content}

        {v

               +--------------------------------+
               |         margin                 |
               |  +----border-----------------+ |
               |  |      padding              | |
               |  |   +---------------------+ | |
               |  |   |                     | | |
               |  |   |                     | | |
               |  |   |      content        | | |
               |  |   |                     | | |
               |  |   +---------------------+ | |
               |  +---------------------------+ |
               |                                |
               +--------------------------------+
        v}
     *)


    val margin: string -> 'msg t
    (** Examples

        {[
            margin "25px"
            margin "25px 50px"              top/bottom 25px, left/right 50px
            margin "25px 50px 75px 100px"   top, right, bottom, left
        ]}

        [margin str] abbreviates [style "margin" str]
    *)


    val padding: string -> 'msg t
    (** Examples

        {[
            padding "25px"
            padding "25px 50px"              top/bottom 25px, left/right 50px
            padding "25px 50px 75px 100px"   top, right, bottom, left
        ]}

        [padding str] abbreviates [style "padding" str]
    *)


    val border_style: string -> 'msg t
    (** Examples

        {[
            border_style "solid"
            border_style "dotted"
            border_style "dashed"
        ]}

        [border_style str] abbreviates [style "border-style" str]
    *)

    val border_width: string -> 'msg t
    (** Examples

        {[
            border_width "3px"
            border_width "thick"
            border_width "medium"
        ]}
    *)

    val border_color: string -> 'msg t
    (** Example

        {[
            border_color "red"
        ]}
    *)


    (** {1 Common attributes} *)

    val id: string -> 'msg t
    (** "id" attribute *)

    val class_: string -> 'msg t
    (** "class" attribute *)

    val href: string -> 'msg t
    (** "href" attribute *)

    val src: string -> 'msg t
    (** "src" attribute *)

    val title: string -> 'msg t
    (** "title" attribute to display tooltips *)


    (** {1 Input elements} *)

    val value: string -> 'msg t
    (** The [value] property of the element (usually an input element)

        Each time the user enters something to the input element (text for input
        type 'text', slider position for input type 'range', date for input type
        'date'), the value property changes. Using the value property in the
        virtual dom overwrites whatever the user has written into the input
        element. Using a 'value' attribute in the virtual dom does *not*
        overwrite the user value.
    *)


    val placeholder: string -> 'msg t

    val on_input: (string -> 'msg) -> 'msg t
end
