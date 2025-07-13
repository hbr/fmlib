module type  VALUE =
sig
    (** Javascript Values


        Javascript values are necessary to comunicate with the javascript world.
        In order to send a message to the surrounding javascript
        (see {!val: Task.send_to_javascript}) a javascript value is needed.
        The following functions can be used to construct arbitrary javascript
        values (no functions, just data).

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


    type t (** Type of a javascript value. *)

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
