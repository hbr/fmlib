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








(** Decode javascript values into ocaml values. *)
module Decode:
sig
    type 'a t = Value.t -> 'a option
    (** ['a t] Type of a decoder which decodes a javascript value into an
        optional object of type ['a].

        Returns an object of type ['a], if the decoder can decode the javascript
        value into an object of type ['a]. Otherwise returns [None].
    *)

    val return: 'a -> 'a t
    val fail:   'a t
    val (let* ): 'a t -> ('a -> 'b t) -> 'b t
    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    val (</>):   'a t -> 'a t -> 'a t
    val map:     ('a -> 'b) -> 'a t -> 'b t
    val null:       'a -> 'a t
    val undefined:  'a -> 'a t
    val float:      float t
    val int:        int t
    val bool:       bool t
    val string:     string t
    val _function:  (Value.t array -> Value.t) t
    val field:      string -> 'a t -> 'a t
    val array:      'a t -> 'a array t
    val option:     'a t -> 'a option t
end


module Main:
sig
    val raise_js:   string -> 'a
    val log_string: string -> unit
    val log_value:  Value.t -> unit

    val export: (string * Value.t) array -> unit

    val named_export: string -> (string * Value.t) array -> unit

    val test: (Value.t -> Value.t) -> unit

    val node_module:
        'state Decode.t
        -> 'msg Decode.t
        -> ('state -> (Value.t -> unit) -> 'msg -> unit)
        -> unit
end
