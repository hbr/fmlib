module type DECODER =
sig
    type value
    type file

    (** Decoder for Javascript Values *)

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


    (** {1 General}
     *)

    type 'a t
    (** ['a t] Type of a decoder which decodes a javascript value into an
        optional object of type ['a].
    *)


    val run: 'a t -> value -> 'a option
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


    val file_list: file list t
    (** Decode a javascript [FileList] object into an ocaml list of [file]. *)


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
