(** Create webworker in the main thread and in the worker thread. *)




(** {1 Overview}




    {2 Basics}

    An application using webworker has two parts:

    - A creator which creates the webworker
    - An implementation of a webworker

    The creator and the worker can communicate only by sending messages. Usually
    in javascript a webworker is created by

    {v
        function callback (msg) {
            // actions on receiving messages from the worker
        }
        var worker = new Worker('url_to_worker.js')
        worker.onmessage = callback

        worker.postMessage (...)    // send a message to the worker
    v}


    Then there is a javascript implementation of the worker.
    {v
        function onmessage (msg) {
            // actions on receiving a message from the creator
            ...
            postMessage (...)       // send a message to the creator
        }

        ...                         // initialization code

        postMessage (...)           // optional initial message to the creator
        ...
    v}

    Both parts the creator part and the implementation part can be implemented
    in ocaml and this module provides functions to support that.





    {2 Creator Code}

    The call
    {[
        let worker = Web_worker.start "url" decode callback
    ]}
    starts a worker whose implementation can be found at ["url"]. [decode] is a
    decoder to decode messages from the webworker and the function [callback]
    processes the decoded messages.

    {[
        decode: 'msg Decode.t

        callback: 'msg -> unit
    ]}

    If the message received from the webworker cannot be decoded, the event will
    be logged via [console.log].


    The webworker has a function [post] to send messages to the worker. I.e. at
    any time you can call
    {[
        Web_worker.post msg worker
    ]}
    where [msg] is an arbitrary javascript value i.e. [msg: Value.t].

    The command
    {[
        Web_worker.terminate worker
    ]}
    terminates the worker.


    {e Warning:} If the html where the code of the creator resides has been
    loaded into the browser with an url like [file://....], and the url of the
    command [Webworker.start url decode callback] is relative, then the browser
    cannot load the worker. The worker code must always be loaded from a server
    (which can be a local server).





    {2 Implementation Code}

    In ocaml the implementation of a web worker has two components:

    - A decoder [decode] to decode messages which are received as javascript
    values into ocaml values.

    - An implementation function of the form

        {[
            let worker (post: Value.t -> unit): 'msg -> unit =
                ... (* initialization code *)
                ...
                fun msg ->
                    ... (* code executed on receiving a decoded message of type
                           ['msg] *)
                    ...
        ]}

    Having the decoder and the worker function, the actual webworker is made by
    the command

    {[
        make decode worker
    ]}

    The [make] function retrieves the javascript function [postMessage] which
    must be available in the implementation environment of a webworker and
    decodes all incoming messages from the creator. [make] calls [worker] in a
    curried form. First to provide it with the retrieve function [postMessage]
    and subsequently on each decoded incoming message.

    If a message from the creator cannot be decoded, then the event is logged in
    the console.
*)




open Base







(** {1 API} *)


(** {2 Creator Code}
*)

type t
(** Type of a web worker in the creator which can receive messages of type
    ['msg] from the actual worker. *)




val start: string -> 'msg Decode.t -> ('msg -> unit) -> t
(** [start url decode callback]

    Create a webworker loaded from [url] and register the callback [callback] to
    receive messages from the webworker. Use the decoder [decode] to decode
    messages received from the webworker.
*)



val post_message: Value.t -> t -> unit
(** [post msg w] Post the message [msg] to the worker [w]. *)



val terminate: t -> unit
(** [terminate worker] Terminate the webworker [worker]. *)






(** {2 Implementation Code}
*)




type 'msg worker_function = (Value.t -> unit) -> 'msg -> unit
(** Type of a worker function.

    A worker function [f] of that type is called in a curried form. In the first
    call, it receives a function to post messages to its creator.
    {[
        let g = f post
    ]}
    In subsequent calls, it receives messages from its creator
    {[
        g msg
    ]}
*)


val make: 'msg Decode.t -> 'msg worker_function -> unit
(** [make decode f]

    Make the actual webworker. [decode] is a decoder for incoming messages. [f]
    is the main function of the worker. [f] is called in a curried form. The
    first call is
    {[
        let g = f post
    ]}
    where [post] is the function to send messages back to the creator of the
    worker.

    Then each time a message is received from the creator of the webworker, the
    message is decoded by [decode]. In case of success with the ocaml object
    [msg] the function call [g msg] is made.

    The function [make] might raise 2 possible exceptions:

    - It cannot find a global object [postMessage] i.e. it does not run in a
    webworker environment.

    - It finds a global object [postMessage] but the object is not a function.
    This indicates as well that it is not executed in a webworker environment.

    If during the execution of the worker messages arrive which cannot be
    decoded successfully by [decode], then this event is logged.
*)









(** {2 Simulation}
*)


(**

    Sometimes it is useful to simulated the behaviour of a webworker in the main
    thread. As opposed to a real webworker, a simulator runs in the main thread
    and therefore can block the event loop for a certain amount of time.

    Recall that a real webworker is started in the main thread by

    {[
        let worker = Web_worker.start "url" decode callback
    ]}

    where [decode] is a decoder for messages received from the worker and
    [callback] is a function which is called on each received message from the
    worker.

    The behaviour of a worker is completely described by the pair [(dec,wfun)]
    where [dec] is a decoder for messages received from the creator and [wfun]
    is a workerfunction.

    The simulator is started from the main thread by

    {[
        let worker = Web_worker.Simulate.start decode callback dec fun
    ]}

    and there are the functions

    {[
        Web_worker.Simulate.post_message msg worker

        Web_worker.Simulate.terminate worker
    ]}

    to send messages to the worker and to terminate the worker.

*)

(** Simulate the behaviour of a webworker in the main thread. *)
module Simulate:
sig
    type t
    (** Type of the simulator of the webworker. *)


    val start:
        'rcv Decode.t
        -> ('rcv -> unit)
        -> 'msg Decode.t
        -> 'msg worker_function
        -> t
    (** [start decode callback dec wfun] Create and start a simulator of the
        webworker whose behaviour is described by the pair [(dec, wfun)].
    *)


    val post_message: Value.t -> t -> unit
    (** [post_message msg w] Send the message [msg] to the webworker simulator
        [w].
    *)

    val terminate: t -> unit
    (** [terminate w] Terminate the webworker simulator [w]. *)
end




(** It is also possible to simulate the behaviour of a web worker written in
    ocaml within javascript code. Let's assume that the webworker is defined by
    the pair [(decode, wfun)]. Then we write an ocaml file with the content

    {[
        let decode = ...

        let wfun = ...

        let _ =
            Web_worker.Simulate.simulate_js "Simulated_worker" decode wfun
    ]}

    and compile it to [worker.js]. Then instead of writing a html file

    {v
    <html>
        <body>
            ...
            <script>
                var worker = new Worker("worker.js")
                worker.onmessage = (msg) => {...}
                worker.postMessage (msg)
            </script>
        </body>
    </html>
    v}

    we write the html file

    {v
    <html>
        <head>
            <script type="text/javascript" src="worker.js"></script>
        </head>
        <body>
            ...
            <script>
                var worker = Simulated_worker ((msg) => ... )
                worker.postMessage (msg)
            </script>
        </body>
    </html>
    v}
*)


val simulate_js:
    string
    -> 'msg Decode.t
    -> 'msg worker_function
    -> unit
(** [simulate_js name decode wfun]

    Create a simulation of a webworker given by [(decode, wfun)] and make it
    accessible from the javascript code under the name [name].

    In javascript you write [var worker = name (callback)] to start the worker
    and [worker.postMessage(msg)] to send a message to the worker. Furthermore
    it is possible to terminate the worker by [worker.terminate()].
*)
