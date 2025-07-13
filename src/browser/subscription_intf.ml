module type S =
sig
    (** Subscriptions to global events. *)


    type time
    type _ decoder

    (** {1 Basics} *)

    type 'm t

    val none: 'm t

    val batch: 'm t list -> 'm t

    val map: ('a -> 'b) -> 'a t -> 'b t


    (** {1 Events on the window object} *)

    val on_window:
        string -> 'm decoder -> 'm t
    (** [on_window event_type decode]

        Subscribe to window events of type [event_type]. Examples

        {[
            on_window "resize"  decode
            on_window "keydown" decode
        ]}
    *)


    (** {1 Incoming messages from javascript} *)

    val on_message: 'm decoder -> 'm t
    (** [on_message decode]

        Subscribe to incoming messages from the javascript world. If there is an
        incoming message (a javascript object) then decode the object with the
        help of the function [decode] and send the decoded value as a message to
        the update function of the application.
    *)


    (** {1 Timer events} *)

    val every: int -> (time -> 'm) -> 'm t
    (** [every millis f] Subscribe to an event which is fired every [millis]
        milliseconds. Use [f] to map the posix time into a message for the
        update function. *)


    val on_animation: (time -> 'm) -> 'm t
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
