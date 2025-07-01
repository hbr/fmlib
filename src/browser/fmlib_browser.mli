(** Web Applications Running in the Browser


    This library helps to write web applications which run in the browser. See
    some simple
    {{: https://hbr.github.io/fmlib/webapp/index.html} live examples} and look
    into the
    {{: https://github.com/hbr/fmlib/tree/master/src/examples/browser/webapp.ml}
       source code}.


    For a step by step introduction see
    {{!page-doc} "Introduction to Web Applications"}.



*)








(** {1 Utilities} *)

(** Generate Random Numbers *)
module Random:
sig
    type 'a t
    (** Generator, generating random values of type ['a]. *)


    val constant: 'a -> 'a t
    (** [constant a] Generate the same value every time. *)

    val (>>=):    'a t -> ('a -> 'b t) -> 'b t
    (** [rand >>= f] Generate the random value [a] using the generator [rand],
        and then use [f a] to generate a random value [b].
     *)


    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    (** [let* a = rand in f a] is the same as [rand >>= f]. *)



    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f rand] Use [rand] to generate a random value and then map it by
        [f]. *)



    val int: int -> int t
    (** [int bound] A random generator which generates numbers [n] satisfying
        [0 <= n < bound].

        Precondition: [0 < bound]
     *)

    val float: float -> float t
    (** [float bound] A random generator which generates numbers [n] satisfying
        [0.0 <= n <= bound].

        Precondition: [0 <= bound]
     *)

    val bool: bool t
    (** Generate a random boolean value. *)

    val choose: 'a list -> 'a t
    (** [uniform lst] Generate a random value of the list [lst].

        Precondition: List must not be empty [lst <> []]
    *)
end








(** Posix Time *)
module Time:
sig
    (** Posix time is the number of milliseconds passed since the beginning of
        the unix epoch i.e. since 1.1.1970 in utc i.e. universal coordinated
        time.

        In order to get the time in your time zone you need a time zone.
     *)


    (** Time zone *)
    module Zone:
    sig
        (** A time zone is the offset in minutes from utc. Time zones westward
            of utc get a positive offset, eastward of utc a negative offset.
         *)

        type t

        val utc: t

        val make: int -> t
        (** [make offset] Time zone [offset] minutes westward of utc.

[make (-60)] is the zone of central european winter time. It is one
            hour eastward of utc.
         *)

        val offset: t -> int
        (** [offset zone] The offset of [zone] in minutes westward of utc. *)
    end



    type t

    val zero: t
    (** [1.1.1970] in utc. *)



    val of_float: float -> t

    val to_float: t -> float



    (** In order to get a year, month, ... you need the utc time and the time
        zone you are in. *)


    val year:    t -> Zone.t -> int
    (** [year time zone] The year of [time] in [zone]. *)


    val month:   t -> Zone.t -> int
    (** [month time zone] The month of [time] in [zone].

        January is month [0].
    *)

    val day_of_month: t -> Zone.t -> int
    (** [day_of_month time zone] The day of the month of [time] in [zone].

        First day of the month is day [0].
    *)

    val hour:    t -> Zone.t -> int
    (** 0 - 23 *)

    val minute:  t -> Zone.t -> int
    (** 0 - 59 *)

    val second:  t -> Zone.t -> int
    (** 0 - 59 *)

    val milli_second:  t -> Zone.t -> int
    (** 0 - 999 *)
end








(** Event flags to stop propagation and prevent default action. *)
module Event_flag:
sig
    type prevent
    type stop

    val prevent:    prevent
    (** Prevent default handling. *)

    val no_prevent: prevent
    (** Do not prevent default handling. *)


    val stop:    stop
    (** Stop event propagation. *)

    val no_stop: stop
    (** Do not stop event propagation. *)
end











(** {1 Encode and Decode Javascript Values} *)



(** Javascript Values


    Javascript values are necessary to comunicate with the javascript world. In
    order to send a message to the surrounding javascript (see
    {!val: Task.send_to_javascript}) a javascript value is needed. The following
    functions can be used to construct arbitrary javascript values (no
    functions, just data).

    E.g. if you want to construct the javascript object

    {v
        {first_name: "John", last_name: "Doe", age: 45}
    v}
    you just write
    {[
        record
            [|
              "first_name", string "John"
            ; "last_name", string "Doe"
            ; "age", int 45
            |]
    ]}

 *)
module Value:
sig
    type t

    val null: t (** The javascript value [null] *)

    val string: string -> t
    (** [string str] The javascript string [str] *)

    val int: int -> t
    (** [int 5] The javascript number [5]. *)

    val bool: bool -> t
    (** [bool true] The javascript value [true]. *)

    val float: float -> t
    (** [float 5] The javascript number [5]. *)

    val record: (string * t) array -> t
    (** [record [| "a", int 5;  "b", string "hello"|]] is the javascript value
        [{a: 5, b: 'hello'}|].
    *)

    val array: t array -> t
    (** [array [|int 5; string "hello"; bool true|] ] is the javascript array
        [[5, "hello", true]].
     *)

    val stringify: t -> t
    (** Serialize the javascript object. The result is a javascript string
        representing the json encoding of the javascript object.
     *)
end








(** Decoder for Javascript Values *)
module Decoder:
sig
    (** {1 Overview}

    Suppose we have the following ocaml types

    {[
        type sex = Male | Female

        type tp = {
            name:   string;
            age:    int;
            sex:    sex
        }
    ]}

    and we want to decode the javascript object

    {[
        {name: "Jonathan", sex: "male", age: 55}
    ]}

    The we can use the following decoder

    {[
        let decode: tp Decode.t =
            let open Decode in
            let* name = field "name" string in
            let* age  = field "age"  int    in
            let* sex  =
                field
                    "sex"
                    (
                        let* str = string in
                        match str with
                        | "male" ->
                            return Male
                        | "female" ->
                            return Female
                        | _ ->
                            fail
                    )
            in
            return {name; age; sex}
    ]}

    The decoder [decode] decodes any javascript object which has the fields
    [name] [age] and [sex] with a value of the appropriate type into the
    corresponding ocaml record.
    *)

    (** {1 API} *)


    (** {1 General}
     *)

    type 'a t
    (** ['a t] Type of a decoder which decodes a javascript value into an
        optional object of type ['a].
    *)


    val run: 'a t -> Value.t -> 'a option
    (** [run decoder value] Run the decoder on a javascript value. If the
        decoder succeeds with value [a], then return [Some a]. Otherwise return
        [None].
    *)


    val return: 'a -> 'a t
    (** [return a] Succeed with [a] regardless what the javascript object is. *)


    val fail: 'a t
    (** Fail immediately. *)


    val (>>=):    'a t -> ('a -> 'b t) -> 'b t
    (** [dec >>= f]

        Equivalent to
        {[
            let* v = dec in
            f v
        ]}
    *)

    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    (** Combine decoders.

        Example:
        {[
            let* a = dec1 in
            dec2 a
        ]}

        First decode the javascript value with decoder [dec1]. In case of
        success with the value [a], use decoder [dec2] which can depend on
        [a].

        [let*] is useful to decode various fields of an object.
        {[
            let* f1 = field "name1" dec1 in
            let* f2 = field "name2" dec2 in
            ...
            return ...
        ]}
    *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f dec] Decode using [dec] and in case of success, map the decoded
        value [a] to [f a]. *)


    val (</>): 'a t -> 'a t -> 'a t
    (** [dec1 </> dec2] First try decoder [dec1]. If it succeeds, return the
        decoded value. In case [dec1] fails, use [dec2] to decode the javascript
        value.
    *)


    (** {1 Basic decoders}
     *)

    val null:      'a -> 'a t
    (** [null v] If the javascript value is [null], then return [v]. Otherwise
        fail.
    *)


    val undefined: 'a -> 'a t
    (** [undefined v] If the javascript value is [undefined], then return [v].
        Otherwise fail.
    *)


    val int: int t
    (** Decode an integer value i.e. a number between [-2^31] and [2^31 - 1].
    *)


    val bool:       bool t
    (** Decode a boolean value. *)


    val float: float t
    (** Decode a floating point value i.e. a number. *)


    val string: string t
    (** Decode a string value. The decoding converts the javascript string from
        utf16 into an ocaml utf8 string.
    *)


    (** {1 Complex decoders}
     *)

    val field: string -> 'a t -> 'a t
    (** [field name dec] Decode the field named [name] in the javascript object
        with the decoder [dec].
    *)


    val array: 'a t -> 'a array t
    (** [array dec] Decode a javascript array into an ocaml array using [dec] to
        decode elements.
    *)


    val option: 'a t -> 'a option t
    (** [option dec] In case the javascript object is [null] succeed with [None].
        Otherwise use [dec] to decode the object and in case of success wrap the
        result with [Some].

        Examples:
        {[
            run (option int) Value.null         ~>      Some None
            run (option int) (Value.int 5)      ~>      Some (Some 5)
            run (option int) (Value.string "a") ~>      None
        ]}
    *)
end






(** {1 Virtual Dom} *)


(** Attributes of Dom Elements.

    There are four types of attributes:

    - style attributes: Color, font size, etc.

    - property attributes: arbitrary javascript values as properties of the
    corresponding dom element.

    - attributes: string valued attributes to give the element an id, a
    classname, etc.

    - handler attributes: React to user interactions (mouse clicks, ...) on the
    html element.

    Sometimes the distinction between properties and attributes is quite subtle.
    To the best of my knowledge each added attribute adds a property with the
    same name (except when the name is a javascript keyword like "class") to the
    html element. But not all properties even  if it is a string value adds
    an attribute to the element.

 *)
module Attribute:
sig
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

    val property: string -> Value.t -> 'msg t
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
        -> Event_flag.stop
        -> Event_flag.prevent
        -> 'msg Decoder.t
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

    val on: string -> 'msg Decoder.t -> 'msg t
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








(** Virtual Dom *)
module Html:
sig
    (** {1 Primitives}
     *)

    type 'msg t (** Type of a virtual dom node potentially generating a message
                    of type ['msg]. *)

    val text: string -> 'msg t (** [text str] Create a text node. *)

    val node: string -> 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [node tag attrs children]

        Create an html element with a tagname, a list of attributes and a list
        of children.
    *)

    val node_ns:
        string -> string -> 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [node namespace tag attrs children]

        Like [node], but creates the node within a namespace e.g.
        "http://www.w3.org/2000/svg" for [svg] elements.
    *)



    val svg_node: string -> 'msg Attribute.t list -> 'msg t list -> 'msg t
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
        string -> 'msg Attribute.t list -> (string * 'msg t) list -> 'msg t
    (** [keyed tag attrs children]

        Like [node], but add a unique identifier to each child node. This makes
        adding, removing and modifying child nodes more efficient. The dom
        diffing algorithm compares child nodes with the same identifier.
     *)



    (** {1 Headers}
     *)

    val h1: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [h1 attrs children] is equivalent to [node "h1" attrs children]. *)

    val h2: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [h2 attrs children] is equivalent to [node "h2" attrs children]. *)

    val h3: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [h3 attrs children] is equivalent to [node "h3" attrs children]. *)

    val h4: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [h4 attrs children] is equivalent to [node "h4" attrs children]. *)

    val h5: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [h5 attrs children] is equivalent to [node "h5" attrs children]. *)

    val h6: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [h6 attrs children] is equivalent to [node "h6" attrs children]. *)




    (** {1 Grouping Content}
     *)

    val div: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [div attrs children] is equivalent to [node "div" attrs children]. *)

    val span: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [span attrs children] is equivalent to [node "span" attrs children]. *)

    val pre: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [pre attrs children] is equivalent to [node "pre" attrs children]. *)

    val p: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [p attrs children] is equivalent to [node "p" attrs children]. *)




    (** {1 Input}
     *)

    val button: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [button attrs children] is equivalent to [node "button" attrs children]. *)

    val input: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [input attrs children] is equivalent to [node "input" attrs children]. *)

    val label: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [label attrs children] is equivalent to [node "label" attrs children]. *)

    val textarea: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [textarea attrs children] is equivalent to
        [node "textarea" attrs children]. *)

    val select: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [select attrs children] is equivalent to [node "select" attrs children]. *)



    (** {1 Lists}
     *)

    val ol: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [ol attrs children] is equivalent to [node "ol" attrs children]. *)

    val ul: 'msg Attribute.t list -> 'msg t list -> 'msg t
    (** [ul attrs children] is equivalent to [node "ul" attrs children]. *)

    val li: 'msg Attribute.t list -> 'msg t list -> 'msg t
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











(** {1 Commands and Subscriptions} *)




(** Tasks to be performed within {{!module:Command} Commands} *)
module Task:
sig
    (** {1 Error types} *)

    type empty = |


    type not_found  = [`Not_found]


    (** {1 Basic type and functions} *)

    type ('a, +'e) t
    (** Task succeeding with a value of type ['a] or failing with
         an error object of type ['e] *)


    val succeed: 'a -> ('a, 'e) t
    (** [succeed a] Task which immediately succeeds with value [a]. *)


    val return:  'a -> ('a, 'e) t
    (** Same as {!succeed}. *)


    val fail: 'e -> ('a, 'e) t
    (** [fail e] Task which immediately fails with the error [e]. *)


    val result: ('a, 'e) result -> ('a, 'e) t
    (** [result res] Task which immediately succeeds or fails depending on [res]

        The effect of the function is described by the code

        {[
            match res with
            | Ok a    -> succeed a
            | Error e -> fail e
        ]}
    *)


    val (>>=): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    (** [task >>= f]

        First execute [task]. If it fails then the function fails. If [task]
        succeeds with the result [a] then execute the task [f a].
    *)


    val ( let* ): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    (** More convenient syntax for the monadic bind operator {!(>>=)}.

        The code
        {[
            let* a = task in
            f a
        ]}

        is equivalent to
        {[
            task >>= f
        ]}

        With the [let*] operator it is more convenient to chain tasks.

        {[
            let* a = t1 in
            let* b = t2 a in
            let* c = t3 a b in
            ...
            return f a b c ...
        ]}
    *)


    val map: ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    (** [map f task] Map the success result of [task] via the function [f]. *)



    val make_succeed: (('a, 'e) result -> 'b) -> ('a, 'e) t -> ('b, empty) t
    (** [make_succeed f task]

        Convert the task which might fail into a task which always succeeds by
        converting the positive or negative result via the function [f] into a
        new result.
    *)




    val parallel:
        'accu -> ('a -> 'accu -> 'accu)
        -> ('a, empty) t list
        -> ('accu, empty) t
    (** [parallel accu_start accumulate task_list]

        Run all the tasks in the task list in parallel. Collect the results of
        the individual tasks via the function [accumulate] into the accumulator.
        If all tasks of the list have finished, return the accumulator.

        Note that the tasks of the list do not return errors. If they can have
        errors then {!make_succeed} can be used to encode the error into the
        result type ['a].
    *)





    (** {1 Write to the console} *)

    val log_string: string -> (unit, 'e) t
    (** [log_string str] Write [str] to the console. *)


    val log_value: Value.t -> (unit, 'e) t
    (** [log_value v] Write the javascript object [v] to the console. *)





    (** {1 Messages to the javascript world} *)

    val send_to_javascript: Value.t -> (unit, 'e) t
    (** [send_to_javascript value] Send the javascript object [value] to the
        surrounding javascript world. *)



    (** {1 Focus and blur elements} *)

    val focus: string -> (unit, not_found) t
    (** [focus id] Put the dom element with [id] into focus. *)

    val blur: string -> (unit, not_found) t
    (** [blur id] Unfocus the dom element with [id]. *)




    (** {1 Defer tasks a certain time} *)

    val sleep: int -> 'a -> ('a, 'e) t
    (** [sleep millis a] Sleep for [millis] milliseconds and then return [a].

        Examples:

        {[
            let* _ = sleep 1000 () in       (* sleep 1000 milliseconds *)
            task                            (* and then execute [task] *)

            let* a = task1 >>= sleep 1000   (* excute [task1] and return result
                                               [a] after 1000 milliseconds *)
            in
            task2 a                         (* then execute [task2 a] *)
        ]}

    *)


    val next_tick: 'a -> ('a, 'e) t
    (** [next_tick a] Return [a] in the next tick of the event loop.

        Example: Execute [task] in the next round of the event loop.
        {[
            let* _ = next_tick () in
            task
        ]}
    *)




    (** {1 Time and time zone} *)

    val now: (Time.t, 'e) t
    (** Get the current time. *)

    val time_zone: (Time.Zone.t, 'e) t
    (** Get the current time zone. *)




    (** {1 Random values} *)

    val random: 'a Random.t -> ('a, 'e) t
    (** [random ran] Execute the random generator [rand] and return the
        generated random value. *)



    (** {1 Http requests} *)

    module Http:
    sig
        type error = [
            | `Status of int
            (**
               - 0: no internet, server not found, timeout, ...
               - 401: bad request
               - 403: forbidden
               - 404: page not found
               - ...

            *)
            | `No_json (** Resource is not a valid json file *)
            | `Decode (** Resource is a valid json file, but the decoder could
                           not decode the corresponding javascript object. *)
        ]


        module Body:
        sig
            type t

            val empty : t
            (** The body will be empty. *)

            val string : string -> string -> t
            (** [string media_type s]

                The body will be the string [s]. The [Content-Type] header
                will be automatically set to the given [media_type]. For common
                media types, a.k.a. MIME types, see
                {{: https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types/Common_types } this list}. *)

            val json : Value.t -> t
            (** [json v]

                The body will be [v], encoded as json. The [Content-Type] header
                will be automatically set to [application/json]. *)

        end


        module Expect:
        sig
            type 'a t

            val string : string t
            (** The response is expected to be a string and will not be decoded
                further. *)

            val json : 'a Decoder.t -> 'a t
            (** [json decoder]

                The response is expected to be json and will be decoded with
                [decoder]. *)
        end


        val request:
            string
            -> string
            -> (string * string) list
            -> Body.t
            -> 'a Expect.t
            -> ('a, error) t
        (** [request method url headers body expect]

            Make an http [method] request to [url] with [headers] and [body].
            [expect] specifies the expected response format.

            This is the most general http request function. See also the more
            specific functions [text] and [json].

            Example:
            {[
                let user = Value.(record [| ("username", string "Bob") |]) in
                request "PUT" "/users" [] (Body.json user) (Expect.string)
                |> Command.attempt (fun result ->
                    match result with
                    | Ok _ ->
                        GotUserCreated
                    | Error _ ->
                        GotError "failed to create user")
            ]}
        *)


        val text:
            string
            -> string
            -> (string * string) list
            -> string
            -> (string, error) t
        (** [text method url headers body]

            Make an http [method] request to [url] with [headers] and a string
            as the [body]. Expect a string as the response.

            Method is one of [GET, POST, DELETE, ... ].

            The headers and the body can be empty. The [Content-Type] header
            is automatically set to [text/plain].

            Example:
            {[
                text "PUT" "/users" [] "Bob"
                |> Command.attempt (fun result ->
                    match result with
                    | Ok _ ->
                        GotUserCreated
                    | Error _ ->
                        GotError "failed to create user")
            ]}
        *)


        val json:
            string
            -> string
            -> (string * string) list
            -> Value.t option
            -> 'a Decoder.t
            -> ('a, error) t
        (** [json method url headers body decoder]

            Make an http [method] request to [url] with [headers] and an
            optional json value as the [body]. Expect a json value as the
            response which will be decoded by [decoder].

            The [headers] can be empty. The [Content-Type] header is
            automatically set to [application/json] if [body] is not [None].

            Example:
            {[
                let decoder = Decoder.array Decoder.string in
                json "GET" "/users" [] None decoder
                |> Command.attempt (fun result ->
                    match result with
                    | Ok usernames -> (* the usernames were successfully decoded
                                         into a string array *)
                        GotUsers (Array.to_list usernames)
                    | Error _ ->
                        GotError "failed to obtain users")
            ]}
         *)
    end
end







(** Commands to be executed as a result of an update operation.

    An elementary command consists of a {!module:Task} to be executed.

*)
module Command:
sig

    (** {1 Basics} *)

    type _ t
    (** [msg t] is the type of a command generating an object of type [msg] to
        inject it into the update function of the application. *)

    val none: _ t
    (** An empty command. *)

    val batch: 'm t list -> 'm t
    (** [batch lst] A list of commands to be executed. *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** Map the message of a command. *)




    (** {1 Simple Commands} *)


    val now: (Time.t -> 'm) -> 'm t
    (** Get the current time. *)


    val time_zone: (Time.Zone.t -> 'm) -> 'm t
    (** Get the time zone. *)



    val focus: string -> 'm t
    (** [focus id]

        Focus the element [id]. If the element does not exist, then nothing is
        done. This command does not return any message.
    *)


    val blur: string -> 'm t
    (** [blur id]

        Blur the element [id]. If the element does not exist, then nothing is
        done. This command does not return any message.
    *)


    val focus_with_info: string -> 'm -> 'm -> 'm t
    (** [focus_with_info id ok not_found]

        Focus the element [id] and return [ok]. Return [not_found], if the
        element does not exist.
    *)


    val blur_with_info: string -> 'm -> 'm -> 'm t
    (** [blur_with_info id ok not_found]

        Blur the element [id] and return [ok]. Return [not_found], if the
        element does not exist.
    *)


    val log_string: string -> 'm t
    (** Print a string to the console, don't return a message. *)


    val log_value: Value.t -> 'm t
    (** Print a value to the console, don't return a message. *)


    val random: 'm Random.t -> 'm t
    (** Generate a random value. *)


    val notify: int -> 'm -> 'm t
    (** [notify millis msg]

        Send [msg] in [millis] milliseconds.
    *)


    val send_to_javascript: Value.t -> 'm t
    (** Send a value to the surrounding javascript code. *)




    (** {1 Execute Tasks} *)

    (** If a command wants to execute chains of simple commands before returning
        a message to the application, then it is necessary to create a task
        which does the more complex operation and perform the task within a
        command.

        An object of type [('a, 'e) Task.t] is a task which in case of success
        returns a value of type ['a] and in case of failure returns a value of
        type ['e].

        An object of type [('a, Task.empty) Task.t] is a task which cannot fail.
    *)


    val attempt: (('a, 'e) result -> 'm) -> ('a, 'e) Task.t -> 'm t
    (** [attempt f task] Attempt the possibly failing [task] and map the result
        via the function [f] into a message to send to the application. *)


    val perform: ('m, Task.empty) Task.t -> 'm t
    (** [perform task] Perform the non failing [task] and send the message
        generated by the task to the application. *)


    val just_do: (unit, Task.empty) Task.t -> 'm t
    (** [perform task] Perform the non failing [task] and don't send any message
        to the application. *)



    (** {1 Reference Nodes }

        More details on reference nodes see {!val:Html.reference}.
    *)


    val set_reference: string -> 'm Html.t -> 'm t
    (** [set_reference name vdom]

        Display [vdom] in the reference node [name].

        If a reference node [name] does not yet exist, then create a reference
        node.
    *)
end





(** Subscriptions to global events. *)
module Subscription:
sig

    (** {1 Basics} *)

    type 'm t

    val none: 'm t

    val batch: 'm t list -> 'm t

    val map: ('a -> 'b) -> 'a t -> 'b t


    (** {1 Events on the window object} *)

    val on_window:
        string -> 'm Decoder.t -> 'm t
    (** [on_window event_type decode]

        Subscribe to window events of type [event_type]. Examples

        {[
            on_window "resize"  decode
            on_window "keydown" decode
        ]}
    *)


    (** {1 Incoming messages from javascript} *)

    val on_message: 'm Decoder.t -> 'm t
    (** [on_message decode]

        Subscribe to incoming messages from the javascript world. If there is an
        incoming message (a javascript object) then decode the object with the
        help of the function [decode] and send the decoded value as a message to
        the update function of the application.
    *)


    (** {1 Timer events} *)

    val every: int -> (Time.t -> 'm) -> 'm t
    (** [every millis f] Subscribe to an event which is fired every [millis]
        milliseconds. Use [f] to map the posix time into a message for the
        update function. *)


    val on_animation: (Time.t -> 'm) -> 'm t
    (** [on_animation f]

        Subscribe to the [requestAnimationFrame] event. The callback gets the
        posix time at the event.
    *)


    (** {1 Keyboard events} *)

    val on_keydown: (string -> 'm) -> 'm t
    val on_keyup:   (string -> 'm) -> 'm t


    (** {1 Mouse events} *)

    (** The following mouse event subscriptions just decode the [clientX] and
        [clientY] value of the mouse event. But a mouse event has much more
        information. You can get a customized subscription which decodes more
        mouse event data by just writing a decoder [decode] which decodes all
        values from a mouse event which are of interest and subscribe to the
        mouse event e.g. by [on_window "mouseup" decode]. *)

    val on_mouse_down: (int -> int -> 'm) -> 'm t
    (** Subscribe to mousedown events. *)

    val on_mouse_move: (int -> int -> 'm) -> 'm t
    (** Subscribe to mousemove events. *)

    val on_mouse_up:   (int -> int -> 'm) -> 'm t
    (** Subscribe to mouseup events. *)



    (** {1 Window resize} *)

    val on_resize: (int -> int -> 'm) -> 'm t
    (** [on_resize f] Subscribe to the window resize event and report the
        [innerWidth] and [innerHeight] properties to the function [f] to generate
        the message for the update function. *)



    (** {1 Visibility change} *)

    val on_visibility_change: (string -> 'm) -> 'm t
    (** [on_visibility_change f] Subscribe to the window visibility
        change event and report the visibility state which is either "visible"
        or "hidden" to the function [f] to generate the message for the update
        function. *)
end





(** {1 Debugging} *)


val debug: string -> unit
(** [debug str] Log [str] to the console as a side effect. *)


val debug_value: Value.t -> unit
(** [debug_value v a] Log the javascript value [v] to the console
    as a side effect. *)










(** {1 Sandbox Applications} *)

(**
    A sandbox application has only limited user interactions. A sandbox
    application cannot execute commands. It can only get messages from user
    interactions like mouse clicks, keyboard strokes on elements in focus etc.

    The dom of a sandbox application is put directly under the [body] of the
    html page i.e. it occupies the whole browser window.
*)

val sandbox:
    'state
    -> ('state -> 'msg Html.t)
    -> ('state -> 'msg -> 'state)
    -> unit
(** [sandbox state view update]

    A sandbox application is started with the command

    {[
        let _ = sandbox state view update
    ]}

    and it needs only a very simple html file of the form

    {v
        <!-- file: index.html -->
        <!DOCTYPE html>
        <html>
            <head>
                <script type="text/javascript" src="webapp.js">
                </script>
            </head>
            <body>
            </body>
        </html>
    v}

    The application is started on the onload event of the browser window.
*)






val sandbox_plus:
    'state
    -> ('state -> 'msg Html.t)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state)
    -> unit
(** [sandbox_plus state view subs update]

    A [sandbox_plus] application is like a sandbox application. In addition it
    can receive notifications.
*)





(** {1 Full Web Application} *)

(**
    A full web application has full user interaction, can execute arbitrary
    commands and subscribe to all possible global events.

*)

val application:
    string
    -> ('state * 'msg Command.t) Decoder.t
    -> ('state -> 'msg Html.t * string)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit
(** [application my_app init view subs update]

    Browser application named [my_app] on the javascript side. The
    application creates the global object named [my_app] which contains the two
    functions [init] and [post].

    The application is started on the javascript side with
    {v

        my_app.init ({
            data: <initialisation object>,
            onMessage: <function to receive messages on the javascript side from
                        the application>
        })
    v}

    The javascript code can post messages to the application by
    {v
        my_app.post (message)
    v}

 *)




val basic_application:
    'state
    -> 'msg Command.t
    -> ('state -> 'msg Html.t * string)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit
(** [basic_application state command view subs update]

    A [basic_application] is like an [application] which cannot interact with
    the surrounding javascript. I.e. it cannot receive initialization date, it
    cannot receive messages and cannot send messages from the javscript world.
 *)








(** {1 Element Application}

    An element application is like {!val: application} above with the difference
    that the dom is inserted directly under a certain element of the dom tree.
    The web application generated by the library does not touch the dom outside
    the user chosen element.

    Purpose of the element application: Use an already written webapplication in
    javascript and add certain functions written in ocaml by using this library.

    The element application offers a smooth path to start using the library
    without rewriting an already existing application from scratch.


    The javascript part and the ocaml part can communicate via message passing
    i.e. the javascript part can post a javascript object to ocaml
    (see {!val: Subscription.on_message}) and the ocaml
    part can send javascript objects (see {!module: Value} and
    {!val: Task.send_to_javascript}).
*)

val element:
    string
    -> ('state * 'msg Command.t) Decoder.t
    -> ('state -> 'msg Html.t)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit
(** [element my_app init view subs update]

    Browser application named [my_app] on the javascript side. The
    application creates the global object named [my_app] which contains the two
    functions [init] and [post].

    The application is started on the javascript side with
    {v

        my_app.init ({
            data: <initialisation object>,
            element_id: <id of the element under which the application works>,
            onMessage: <function to receive messages on the javascript side from
                        the application>
        })
    v}

    The javascript code can post messages to the application by
    {v
        my_app.post (message)
    v}

 *)
