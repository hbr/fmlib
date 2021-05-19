(** Basic functions for the interaction between ocaml and javascript.
*)


(** Generate javascript values from ocaml values. *)
module Value:
sig
    type t
    (** Type of a javascript value. *)

    val null: t
    (** The javascript value [null]. *)

    val undefined: t
    (** The javascript value [undefined]. *)

    val int: int -> t
    (** [int i] The integer [i] as a javascript value.

        This function is the identity function, because [int] is represented in
        javascript as a number which is a 64 bit floating point value.

        As long as we stay within ocaml code, 32 bit signed integer
        arithmetics is done.
    *)

    val float: float -> t
    (** [float v] The floating point value [v] as a javascript value.

        This function is the identity function, because [float] is represented
        in javascript as a number which is a 64 bit floating point value.
    *)

    val bool: bool -> t
    (** [bool b] Convert the ocaml boolean value [b] into a javascript boolean
        value. *)


    val string: string -> t
    (** [string s] Convert the ocaml string [s] into a javascript string.

        In ocaml a string is a sequence of bytes and in javascript a string is a
        sequence of utf16 code points.
    *)

    val _object: (string * t) array -> t
    (** [_object [|name1, value1; name2, value2; ...|]]

        Make the javascript object [{name1: value1, name2: value2, ...}].
    *)


    val array: t array -> t
    (** [array arr] Convert the ocaml array [arr] of javascript objects into the
        corresponding javascript array. *)


    val function1: (t -> t) -> t
    (** [function1 f] Convert the ocaml function [f] of type [t -> t] into the
        corresponding javascript function.

        No actual conversion is necessary, because ocaml functions are compiled
        to javascript functions.

        However javascript cannot do currying i.e. partial application. In
        javascript the corresponding function can be called with zero or more
        arguments.

        If there are arguments missing, then the function will be called with
        sufficient arguments filled up by [undefined].

        If more arguments are supplied, then the superfluous arguments are
        ignored.
    *)


    val function2: (t -> t -> t) -> t
    (** [function2 f] See {! function1}, just that here [f] is a two argument
        function. *)

    val function3: (t -> t -> t -> t) -> t
    (** [function3 f] See {! function1}, just that here [f] is a three argument
        function. *)
end








(** Decode javascript values into ocaml values.
*)
module Decode:
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

    type 'a t = Value.t -> 'a option
    (** ['a t] Type of a decoder which decodes a javascript value into an
        optional object of type ['a].

        Returns an object of type ['a], if the decoder can decode the javascript
        value into an object of type ['a]. Otherwise returns [None].
    *)

    val return: 'a -> 'a t
    (** [return a] Return [a] regardless what the javascript object is. *)

    val fail:   'a t
    (** Immediately fail i.e. return [None]. *)

    val (let* ): 'a t -> ('a -> 'b t) -> 'b t
    (** Combinate decoders.

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

    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    (** [dec >>= f]

        Equivalent to
        {[
            let* v = dec in
            f v
        ]}
    *)

    val (</>):   'a t -> 'a t -> 'a t
    (** [dec1 </> dec2] First try decoder [dec1]. If it succeeds, return the
        decoded value. In case [dec1] fails, use [dec2] to decode the javascript
        value.
    *)


    val map:     ('a -> 'b) -> 'a t -> 'b t
    (** [map f dec] Decode using [dec] and in case of success, map the decoded
        value by [f]. *)


    val null:       'a -> 'a t
    (** [null v] If the javascript value is [null], then return [v]. Otherwise
        fail.
    *)

    val undefined:  'a -> 'a t
    (** [undefined v] If the javascript value is [undefined], then return [v].
        Otherwise fail.
    *)


    val float:      float t
    (** Decode a floating point value i.e. a number. *)

    val int:        int t
    (** Decode an integer value i.e. a number between [-2^31] and [2^31 - 1].
    *)

    val bool:       bool t
    (** Decode a boolean value. *)


    val string:     string t
    (** Decode a string value. The decoding converts the javascript string from
        utf16 into an ocaml utf8 string.
    *)


    val _function:  (Value.t array -> Value.t) t
    (** Decode a javascript function into an ocaml function.

        Since javascript function can accept any number of arguments, the
        returned function is an ocaml function which accepts an array of
        javascript values and returns a javascript value.

        The function can use decoders to decode its arguments and use the module
        {!module: Value} to generate the result.
    *)


    val _method: (Value.t -> Value.t array -> Value.t) t
    (** Decode a javascript method into an ocaml function.

        The decoded function accepts a javascript value which is bound to [this]
        and and array of javascript value arguments.

        A call [f obj args] is compiled to the javascript call
        [f.apply(obj,args)].

        If the javascript function is not a method, then it ignores the object
        argument.
    *)


    val field:      string -> 'a t -> 'a t
    (** [field name dec] Decode the field named [name] in the javascript object
        with the decoder [dec].
    *)


    val array:      'a t -> 'a array t
    (** [array dec] Decode a javascript array into an ocaml array using [dec] to
        decode elements.
    *)

    val option:     'a t -> 'a option t
    (** [option dec] In case the javascript object is [null] return [Some None].
        Otherwise use [dec] to decode the object and in case of success wrap the
        result with [Some].

        Examples:
        {[
            (option int) Value.null         ~>      Some None
            (option int) (Value.int 6)      ~>      Some (Some 5)
            (option int) (Value.string "a") ~>      None
        ]}
    *)
end




(** Some global functions. *)

module Main:
sig
    (** {1 Javascript Exceptions}
    *)

    val raise_js:   string -> 'a
    (** [raise_js error] Raise a javascript exception. *)


    (** {1 Logging} *)

    val log_string: string -> unit
    (** [log_string str] Log the string [str] via [console.log]. *)

    val log_value:  Value.t -> unit
    (** [log_value v] Log the javascript value [v] via [console.log]. *)





    (** {1 Node Module}

        The functions in this section allow the creation of node modules which
        can interact with the surrounding javascript code. The communication is
        based on message passing. The javascript side can send messages into the
        node module implemented in ocaml and the ocaml side can send messages to
        the javascript side.

        In order to start the node module, initialisation data can be provided.

    *)


    (** The implementation of the node module on the ocaml side is done by
        providing a function of the following type.
    *)

    type ('state,'msg) node_function =
        'state -> (Value.t -> unit) -> 'msg -> unit
    (** Type of the implementation function of the node module.

        The implementation function has the form

        {[
            let node (state: 'state) (callback: Value.t -> unit): 'msg -> unit =
                ... (* initialisation code *)
                ...
                fun msg ->
                    ... (* receive a message from the javascript side *)
                    ...
        ]}
    *)


    (** Having a node function and a state and a message decoder, a node module
        is generated by a call to the following function.
    *)

    val node_module:
        'state Decode.t
        -> 'msg Decode.t
        -> ('state, 'msg) node_function
        -> unit
    (** [node_module state_decode msg_decode node_function]

        An ocaml program [my_app.ml] with an ocaml statement of the form
        {[
            let _ =
                node_module state_decode msg_decode node_function
        ]}
        compiles to [my_app.js] which can be loaded as a module in a nodejs
        script via

        {v
           var my_app = require('./my_app.js')
        v}

        The javascript value [my_app] is a javascript object of the form

        {v
           {init: (initial_state, callback) => { ... }}
        v}

        which can be used in the following way:

        {v
            function callback (msg) {
                ...     // actions on receiving a message from 'my_app'
            }
            var state =
                     ... // initial state which can be decoded
                         // by 'state_decode'

            var post_to_my_app = my_app.init(state, callback)

            post_to_my_app (msg)    // send message to 'my_app',
                                    // must be decodable via 'msg_decode'
        v}

    *)









    (** {1 Browser Application}

        The function in this section allow the creation of browser applications
        which can interact with the surrounding javascript code. The
        communication is base on message passing. The javascript side can send
        messages into the ocaml browser application and vice versa.
    *)

    (** The implementation of a browser application with javascript interop on
        the ocaml side is done by providing a function of the following type.
    *)

    type ('state, 'msg) browser_function =
        'state -> string option -> (Value.t -> unit) -> 'msg -> unit
    (** Type of the implementation function of the browser application with
        javascript interop. *)


    (** The browser function has the form

        {[
            let browser
                (state: 'state)                 (* initial state *)
                (element: string option)        (* optional element id *)
                (callback: Value.t -> unit)     (* send messages to javascript *)
                : 'msg -> unit
                =
                ... (* initialisation code *)
                ...
                fun msg ->
                    ... (* receive a message from the javascript side *)
                    ...
        ]}
    *)

    val browser_application:
        string
        -> 'state Decode.t
        -> 'msg Decode.t
        -> ('state, 'msg) browser_function
        -> unit
    (** [browser_application state_decode msg_decode browser_function]

        An ocaml program [my_app.ml] with an ocaml statement of the form

        {[
            let _ =
                browser_application
                    "my_app"            (* Unique name in the browser *)
                    state_decode
                    msg_decode
                    browser_function
        ]}

        can be compiled to a javascript file [my_app.js]. This file [my_app.js]
        can be included within a html page in a script tag

        {v
            <script type="text/javascript" src="my_app.js"></script>
        v}

        In the html file we need some javascript code to interact with the
        browser application.

        {v  <script>
            var state = ...         // javascript value which can be decoded
                                    // by 'state_decode'

            var postMessage         // variable representing a function to
                                    // post messages to 'my_app'

            var callback (msg) {
                ...                 // actions on receiving messages from
                ...                 // 'my_app'
                ...
                postMessage (...)   // send a message to 'my_app'
            }
            </script>
        v}

        At the end of the body the application [my_app] can be started by

        {v
            <script>
                postMessage = my_app.init (state, 'element_id', callback)
            </script>
        v}

        {e Warning:} The ocaml browser application shall not be initialized
        before the body and in particular the element is available. Therefore
        it is best to initialize the application at the end of the html body.


        It is convenient to initialize the browser application with an element
        id below which the browser application should install itself in the dom
        tree.  This can be used to avoid conflicting dom accesses between the
        javascript side and the ocaml browser application.
    *)



    (** {1 Global Environment} *)

    (**

       If your application written in ocaml wants to communicate with javascript
       code, then the following functions might be interesting.

       With the function {!make_global} it is possible to make a javascript
       value generated via ocaml available to the surrounding javascript code.
       In many cases you will make a function globally available to the
       javascript code. This function can receive initialization data and
       callback functions. With the callback functions you can post messages to
       the surrounding javascript code. Furthermore the globally accessible
       function might return a function which can be used by the surrounding
       javscript code to post messages to the ocaml application.

       If you write a web worker, you have to make function with the name
       [onmessage] available to the global environment. This function has type
       [Value.t -> Value.t] and it usually returns [Value.undefined]. Via
       {!get_global} you can find out, if a function named [postMessage] exists
       (it exists in the global environment of a webworker) and then you can use
       this function to send messages to the creator of the web worker.
    *)

    val make_global: string -> Value.t -> unit
    (** [make_global name value] Make the javascript value [value] accessible
        from javascript code via the global name [name].

        Caution:

            If the global name [name] already exists, it will be overwritten.
            This has fatal consequences if you overwrite e.g. [setTimeout] or
            [document] or other other used global variables or global functions.

            It is recommended to use some prefix like [my_app_...] in [name] in
            order to not pollute the global namespace.
    *)


    val get_global: string -> Value.t option
    (** [get_global name] Check, if [name] exists in the global enviroment and
        if yes, return the corresponding javascript value. Use {!module: Decode}
        to check, if the global is a function, an object, a number etc.
    *)


    val new_global: string -> Value.t array -> Value.t
    (** [new_global constructor args]

        Construct a new javascript object using the constructor [constructor]
        feeding it with the arguments [args].

        Precondition: The constructor must exist and it has to accept the
        arguments. If not, an exception is thrown.
    *)

    val export: (string * Value.t) array -> unit
    (** [export [| name1, val1; name2, val2; ... |]]

        Export the javascript object consisting of the fields [name1], [name2],
        ... to the surrounding javascript code.

        This function is used if you want to write a module to be used in a node
        application. In the javascript code you write
        {[
            var my_app = require('./my_app.js')
        ]}
        and then the javascript variable [my_app] is an object containing all
        the exported fields.
    *)
end
