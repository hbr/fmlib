(** Web Applications Running in the Browser


    This library helps to write web applications which run in the browser. See
    some simple
    {{: https://hbr.github.io/fmlib/webapp/index.html} live examples} and their
    {{: https://github.com/hbr/fmlib/tree/master/src/examples/browser/webapp.ml}
       source code}.


    For a step by step introduction see
    {{!page-doc} "Introduction to Web Applications"}.



*)








(** {1 Utilities} *)

module Random: Random_intf.RANDOM

module Time: Time_intf.TIME

module File: File_intf.FILE

module Event_flag: Event_flag_intf.FLAG





(** {1 Encode and Decode Javascript Values} *)


module Value: Value_intf.VALUE


module Decoder: Decoder_intf.DECODER
    with type value  := Value.t
     and type file   := File.t





(** {1 Http} *)

module Http: Http_intf.S
    with type file       := File.t
     and type value      := Value.t
     and type 'a decoder := 'a Decoder.t



(** {1 Virtual Dom} *)

module Attribute: Attribute_intf.S
    with type 'm decoder := 'm Decoder.t
     and type value      := Value.t
     and type stop       := Event_flag.stop
     and type prevent    := Event_flag.prevent
     and type file       := File.t

module Html: Html_intf.S with type 'm attr := 'm Attribute.t









(** {1 Commands and Subscriptions} *)


module Task: Task_intf.S
    with type 'a random      := 'a Random.t
     and type time           := Time.t
     and type time_zone      := Time.Zone.t
     and type file           := File.t
     and type value          := Value.t
     and type 'a decoder     := 'a Decoder.t
     and type http_error     := Http.error
     and type http_body      := Http.Body.t
     and type 'a http_expect := 'a Http.Expect.t




module Command: Command_intf.S
    with type time       := Time.t
     and type time_zone  := Time.Zone.t
     and type 'a random  := 'a Random.t
     and type value      := Value.t
     and type file       := File.t
     and type 'm html    := 'm Html.t
     and type empty          := Task.empty
     and type read_failed    := Task.read_failed
     and type http_error     := Http.error
     and type http_body      := Http.Body.t
     and type 'a http_expect := 'a Http.Expect.t
     and type ('a,'e) task := ('a, 'e) Task.t



module Subscription: Subscription_intf.S
    with type time       := Time.t
     and type 'a decoder := 'a Decoder.t








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
    can subscribe to events.
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
