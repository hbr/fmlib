module type S =
sig
    (** Tasks to be performed within {{!module:Command} Commands} *)

    type _ random
    type time
    type time_zone
    type file
    type value
    type _ decoder
    type http_error
    type http_body
    type _ http_expect

    (** {1 Error types} *)

    type empty = |


    type not_found  = [`Not_found]


    type read_failed = [`Read_failed]



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


    val log_value: value -> (unit, 'e) t
    (** [log_value v] Write the javascript object [v] to the console. *)





    (** {1 Messages to the javascript world} *)

    val send_to_javascript: value -> (unit, 'e) t
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

    val now: (time, 'e) t
    (** Get the current time. *)

    val time_zone: (time_zone, 'e) t
    (** Get the current time zone. *)




    (** {1 Random values} *)

    val random: 'a random -> ('a, 'e) t
    (** [random ran] Execute the random generator [rand] and return the
        generated random value. *)




    (** {1 File operations} *)

    val select_file: string list -> (file, empty) t
    (** [select_file media_types]
        Show the browser's file selection dialog and return a file when the user
        selected one. The given list of [media_types] allows restricting what
        file types are visible in the dialog (users can still select different
        file types if they want to).

        NOTE: This task only works if it is triggered in reaction to a user
        event, such as a mouse click. This restriction is imposed by browsers
        for security reasons (websites should not be able to ask for file access
        without user interaction).
    *)

    val select_files: string list -> (file list, empty) t
    (** [select_files media_types]
        The same as {!select_file} but allows selecting multiple files at once.

        NOTE: This task only works if it is triggered in reaction to a user
        event, such as a mouse click. This restriction is imposed by browsers
        for security reasons (websites should not be able to ask for file access
        without user interaction).
    *)

    val file_text: file -> (string, read_failed) t
    (** [file_text file f]

        Read the contents of [file] into a string. Reading can fail, e.g. in
        case of missing filesystem permissions.
    *)




    (** {1 Http requests} *)

    val http_request:
        string
        -> string
        -> (string * string) list
        -> http_body
        -> 'a http_expect
        -> ('a, http_error) t
    (** [http_request method url headers body expect]

        Make an http [method] request to [url] with [headers] and [body].
        [expect] specifies the expected response format.

        This is the most general http request function. See also the more
        specific functions [text] and [json].

        Example:
        {[
            let user = Value.(record [| ("username", string "Bob") |]) in
            http_request "PUT" "/users" [] (Body.json user) (Expect.string)
            |> Command.attempt (fun result ->
                match result with
                | Ok _ ->
                    GotUserCreated
                | Error _ ->
                    GotError "failed to create user")
        ]}
    *)

    val http_text:
        string
        -> string
        -> (string * string) list
        -> string
        -> (string, http_error) t
    (** [http_text method url headers body]

        Make an http [method] request to [url] with [headers] and a string
        as the [body]. Expect a string as the response.

        Method is one of [GET, POST, DELETE, ... ].

        The headers and the body can be empty. The [Content-Type] header
        is automatically set to [text/plain].

        Example:
        {[
            http_text "PUT" "/users" [] "Bob"
            |> Command.attempt (fun result ->
                match result with
                | Ok _ ->
                    GotUserCreated
                | Error _ ->
                    GotError "failed to create user")
        ]}
    *)

    val http_json:
        string
        -> string
        -> (string * string) list
        -> value option
        -> 'a decoder
        -> ('a, http_error) t
        (** [http_json method url headers body decoder]

            Make an http [method] request to [url] with [headers] and an
            optional json value as the [body]. Expect a json value as the
            response which will be decoded by [decoder].

            The [headers] can be empty. The [Content-Type] header is
            automatically set to [application/json] if [body] is not [None].

            Example:
            {[
                let decoder = Decoder.array Decoder.string in
                http_json "GET" "/users" [] None decoder
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
