(** Web Applications Running in the Browser *)


(** {1 Documentation}

    {{!page-browser} Introduction to Web Applications}

*)






(** {1 API} *)


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
    (** [uniform a lst] Generate a random value of the list [lst].
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











(** Javascript Values *)
module Value:
sig
    type t

    val null: t

    val string: string -> t

    val int: int -> t

    val bool: bool -> t

    val float: float -> t

    val record: (string * t) array -> t

    val array: t array -> t
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


    (** {2 General}
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
            let* a = decoder1 in
            decoder1 a
        ]}

        First decode the javascript value with decoder [decoder1]. In case of
        success with the value [a], use decoder [decoder2] which can depend on
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


    (** {2 Basic decoders}
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


    (** {2 Complex decoders}
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
            (option int) Value.null         ~>      succeed with None
            (option int) (Value.int 6)      ~>      succeed with (Some 5)
            (option int) (Value.string "a") ~>      fail
        ]}
    *)
end














(** Tasks to be performed within {{!module:Command} Commands} *)
module Task:
sig
    (** {1 Error types} *)

    type empty = []

    type http_error = [
        | `Http_status of int
        (**
                - 0: no internet, server not found, timeout, ...
                - 401: bad request
                - 403: forbidden
                - 404: page not found
                - ...

            *)
        | `Http_no_json (** Resource is not a valid json file *)
        | `Http_decode (** Resource is a valid json file, but the decoder could
                           not decode the corresponding javascript object. *)
    ]

    type not_found  = [`Not_found]


    (** {1 Basic type and functions} *)

    type ('a, +'e) t
    (** Task succeeding with a value of type ['a] or failing with
         an error object of type ['e] *)

    val succeed: 'a -> ('a, 'e) t

    val return:  'a -> ('a, 'e) t

    val fail: 'e -> ('a, 'e) t

    val result: ('a, 'e) result -> ('a, 'e) t

    val (>>=): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val ( let* ): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val map: ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t


    (** {1 Write to the console} *)

    val log_string: string -> (unit, 'e) t
    (** [log_string str] Write [str] to the console. *)


    val log_value: Value.t -> (unit, 'e) t
    (** [log_value v] Write the javascript object [v] to the console. *)





    (** {1 Send messages to the javascript world} *)

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




    (** {1 Get time and time zone} *)

    val now: (Time.t, 'e) t
    (** Get the current time. *)

    val time_zone: (Time.Zone.t, 'e) t
    (** Get the current time zone. *)




    (** {1 Generate random values} *)

    val random: 'a Random.t -> ('a, 'e) t
    (** [random ran] Execute the random generator [rand] and return the
        generated random value. *)



    (** {1 Make http requests} *)

    val http_text:
        string
        -> string
        -> (string * string) list
        -> string
        -> (string, http_error) t
    (** [http_text method url headers body]

        Make a http [method] request to [url] with [headers] and [body]. Expect
        the response as a string.

        Method is one of [GET, POST, DELETE, ... ].

        Then headers and the body can be empty.
    *)


    val http_json:
        string
        -> string
        -> (string * string) list
        -> string
        -> 'a Decoder.t
        -> ('a, http_error) t
end







(** Commands to be executed as a result of an update operation.

    An elementary command consists of a {!module:Task} to be executed.

*)
module Command:
sig
    type _ t
    (** [msg t] is the type of a command generating an object of type [msg] to
        inject it into the update function of the application. *)

    val none: _ t
    (** An empty command. *)

    val batch: 'm t list -> 'm t
    (** [batch lst] A list of commands to be executed. *)


    val perform: ('m, Task.empty) Task.t -> 'm t
    (** [perform task] Perform the non failing [task] and send the message
        generated by the task to the application. *)

    val attempt: (('a, 'e) result -> 'm) -> ('a, 'e) Task.t -> 'm t
    (** [attemp f task] Attempt the possibly failing [task] and map the result
        via the function [f] into a message to send to the application. *)
end





(** Subscriptions to global events. *)
module Subscription:
sig

    (** {1 Basics} *)

    type 'm t

    val none: 'm t

    val batch: 'm t list -> 'm t

    val map: ('a -> 'b) -> 'a t -> 'b t


    (** {1 Generic subscription to windows events} *)

    val on_window:
        string -> 'm Decoder.t -> 'm t
    (** [on_window event_type decode]

        Subscribe to window events of type [event_type]. Examples

        {[
            on_window "resize"  decode
            on_window "keydown" decode
        ]}
    *)


    (** {1 Subscribe to incoming messages from javascript} *)

    val on_message: 'm Decoder.t -> 'm t
    (** [on_message decode]

        Subscribe to incoming messages from the javascript world. If there is an
        incoming message (a javascript object) then decode the object with the
        help of the function [decode] and send the decoded value as a message to
        the update function of the application.
    *)


    (** {1 Subscribe to timer events} *)

    val every: int -> (Time.t -> 'm) -> 'm t
    (** [every millis f] Subscribe to an event which is fired every [millis]
        milliseconds. Use [f] to map the posix time into a message for the
        update function. *)


    (** {1 Subscribe to keyboard events} *)

    val on_keydown: (string -> 'm) -> 'm t


    (** {1 Subscribe to mouse events} *)

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



    (** {1 Subscribe to the window resize event} *)

    val on_resize: (int -> int -> 'm) -> 'm t
    (** [on_resize f] Subscribe to the window resize event and report the
        [innerWidth] and [innerHeight] properties to the function [f] to generate
        the message for the update function. *)



    (** {1 Subscribe to the window visibility change event} *)

    val on_visibility_change: (string -> 'm) -> 'm t
    (** [on_visibility_change f] Subscribe to the window visibility
        change event and report the visibility state which is either "visible"
        or "hidden" to the function [f] to generate the message for the update
        function. *)
end






(** Attributes of Dom Elements. *)
module Attribute:
sig
    (** {1 Generic Interface}
     *)

    type 'msg t
    val style: string -> string -> 'msg t
    val property: string -> Value.t -> 'msg t
    val attribute: string -> string -> 'msg t
    val handler:
        string
        -> Event_flag.stop
        -> Event_flag.prevent
        -> 'msg Decoder.t
        -> 'msg t


    (** {1 Handler} *)

    val on: string -> 'msg Decoder.t -> 'msg t

    val on_click: 'msg -> 'msg t




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



    (** {2 Margin, border, padding and content}

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


    (** {1 Attributes for input elements} *)

    val value: string -> 'msg t
    val placeholder: string -> 'msg t

    val on_input: (string -> 'msg) -> 'msg t
end








(** Virtual Dom *)
module Html:
sig
    (** {1 Primitives}
     *)

    type 'msg t

    val text: string -> 'msg t
    val node: string -> 'msg Attribute.t list -> 'msg t list -> 'msg t


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
end



val sandbox:
    'state
    -> ('state -> 'msg Html.t)
    -> ('state -> 'msg -> 'state)
    -> unit





val element:
    string
    -> ('state * 'msg Command.t) Decoder.t
    -> ('state -> 'msg Html.t)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit
(** [element my_app init view subs update]

    Create a browser application named [my_app] on the javascript side. The
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




val application:
    string
    -> ('state * 'msg Command.t) Decoder.t
    -> ('state -> 'msg Html.t * string)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit
(** [application my_app init view subs update]

    Create a browser application named [my_app] on the javascript side. The
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
