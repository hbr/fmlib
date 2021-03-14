module Value:
sig
    type t
    val null: t
    val undefined: t
    val int: int -> t
    val float: float -> t
    val bool: bool -> t
    val string: string -> t
    val _object: (string * t) array -> t
    val array: t array -> t
    val function1: (t -> t) -> t
    val function2: (t -> t -> t) -> t
end


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
    val option:  'a t -> 'a option t
    val float:   float t
    val int:     int t
    val bool:    bool t
    val string:  string t
    val _function: (Value.t array -> Value.t) t
    val field:   string -> 'a t -> 'a t
    val array:   'a t -> 'a array t
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
