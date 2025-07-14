module type S =
sig
    (** Commands to be executed.

        An elementary command consists of a {!module:Task} to be executed.

    *)

    type time
    type time_zone
    type _ random
    type value
    type file
    type empty
    type read_failed
    type http_error
    type _ http_expect
    type http_body
    type (_, _) task
    type _ html


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


    (** {2 Time and Time Zone} *)


    val now: (time -> 'm) -> 'm t
    (** Get the current time. *)


    val time_zone: (time_zone -> 'm) -> 'm t
    (** Get the time zone. *)



    (** {2 Focus and Blur} *)

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



    (** {2 File Operations} *)

    val select_file: string list -> (file -> 'm) -> 'm t
    (** [select_file media_types f]
        Show the browser's file selection dialog and produce [f file] when the
        user selected a file. The given list of [media_types] allows restricting
        what file types are visible in the dialog (users can still select
        different file types if they want to).

        NOTE: This command only works if it is triggered in reaction to a user
        event, such as a mouse click. This restriction is imposed by browsers
        for security reasons (websites should not be able to ask for file access
        without user interaction).
    *)

    val select_files: string list -> (file list -> 'm) -> 'm t
    (** [select_files media_types f]
        The same as {!select_file} but allows selecting multiple files at once.

        NOTE: This command only works if it is triggered in reaction to a user
        event, such as a mouse click. This restriction is imposed by browsers
        for security reasons (websites should not be able to ask for file access
        without user interaction).
    *)

    val file_text: file -> ((string, read_failed) result -> 'm) -> 'm t
    (** [file_text file f]

        Read the contents of [file] into a string [result] and produce the
        message [f result] when reading has finished. Reading can fail, e.g. in
        case of missing filesystem permissions.
    *)



    (** {2 Logging to the Console} *)

    val log_string: string -> 'm t
    (** Print a string to the console, don't return a message. *)


    val log_value: value -> 'm t
    (** Print a value to the console, don't return a message. *)



    (** {2 Random Values} *)


    val random: 'm random -> 'm t
    (** Generate a random value. *)



    (** {2 Send Messages} *)

    val notify: int -> 'm -> 'm t
    (** [notify millis msg]

        Send [msg] in [millis] milliseconds.
    *)


    val send_to_javascript: value -> 'm t
    (** Send a value to the surrounding javascript code. *)



    (** {2 Reference Nodes }

        More details on reference nodes see {!val:Html.reference}.
    *)


    val set_reference: string -> 'm html -> 'm t
    (** [set_reference name vdom]

        Display [vdom] in the reference node [name].

        If a reference node [name] does not yet exist, then create a reference
        node.
    *)



    (** {2 Http Requests} *)

    val http_request:
        string
        -> string
        -> (string * string) list
        -> http_body
        -> 'm http_expect
        -> (http_error -> 'm)
        -> 'm t
    (** [http_request method url headers body expect error]

        Details see {!val:Task.http_request}
    *)





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


    val attempt: (('a, 'e) result -> 'm) -> ('a, 'e) task -> 'm t
    (** [attempt f task] Attempt the possibly failing [task] and map the result
        via the function [f] into a message to send to the application. *)


    val perform: ('m, empty) task -> 'm t
    (** [perform task] Perform the non failing [task] and send the message
        generated by the task to the application. *)


    val just_do: (unit, empty) task -> 'm t
    (** [perform task] Perform the non failing [task] and don't send any message
        to the application. *)

end
