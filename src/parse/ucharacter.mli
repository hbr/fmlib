open Fmlib_std.Interfaces

(** Parser for streams of unicode characters.

    There are several possibilities to encode unicode characters in byte
    streams.

    - utf8: Encodes a unicode character in 1 to 4 bytes. The ascii characters
    are included as a special case. Mostly used to transfer unicode text data on
    the internet and on unix based platforms (like MacOS).

    - utf16: Encodes a unicode character in 2 or 4 bytes. The whole basic
    mulilingual plane is encoded in 2 bytes and all the other planes need 4
    bytes. Mostly used on windows platforms and in javascript. For text streams
    big and littly endian has to be distinguished.

    There are the following modules available:

    - {!module:Make_utf8}: Parse text streams encoded in [utf-8].

    - {!module:Make_utf16_be}: Parse text streams encoded in [utf-16] big
    endian.

    - {!module:Make_utf16_le}: Parse text streams encoded in [utf-16] little
    endian.

    - {!module:Make}: Parse text streams in any encoding. The encoder and
    decoder have to be provided as module parameter.

    All parsers in this module work like a character parser (see
    {!module:Character.Make}) with some additional combinators to recognize
    unicode characters.
*)




(** Parse an input stream consisting of unicode characters encoded in utf-8. *)
module Make_utf8 (State: ANY) (Final: ANY) (Semantic: ANY):
sig

    (**
    - [State]: User state.
    - [Final]: Final result type of the parser.
    - [Semantic]: Semantic error message (triggered by [fail error])
    *)


    include Ucharacter_intf.UC
        with type token := Utf8.Decoder.t
         and type final := Final.t
         and type state := State.t
         and type semantic := Semantic.t
    (** @inline *)
end












(** Parse an input stream consisting of unicode characters encoded in utf-16 big
    endian. *)
module Make_utf16_be (State: ANY) (Final: ANY) (Semantic: ANY):
sig

    (**
    - [State]: User state.
    - [Final]: Final result type of the parser.
    - [Semantic]: Semantic error message (triggered by [fail error])
    *)

    include Ucharacter_intf.UC
        with type token := Utf16.Be.Decoder.t
         and type final := Final.t
         and type state := State.t
         and type semantic := Semantic.t
    (** @inline *)
end












(** Parse an input stream consisting of unicode characters encoded in utf-16
    little endian. *)
module Make_utf16_le (State: ANY) (Final: ANY) (Semantic: ANY):
sig

    (**
    - [State]: User state.
    - [Final]: Final result type of the parser.
    - [Semantic]: Semantic error message (triggered by [fail error])
    *)

    include Ucharacter_intf.UC
        with type token := Utf16.Le.Decoder.t
         and type final := Final.t
         and type state := State.t
         and type semantic := Semantic.t
    (** @inline *)
end







(** Parse an input stream consisting of unicode characters. The unicode
    characters are encoded and decoded by using the module [Codec]. *)
module Make
        (Codec: Interfaces.CHAR_CODEC)
        (State:  ANY)
        (Final: ANY)
        (Semantic: ANY) :
sig

    (**
    - [Codec]: Encoder and decoder for unicode characters
    - [State]: User state.
    - [Final]: Final result type of the parser.
    - [Semantic]: Semantic error message (triggered by [fail error])
    *)

    include Ucharacter_intf.UC
        with type token := Codec.Decoder.t
         and type final := Final.t
         and type state := State.t
         and type semantic := Semantic.t
    (** @inline *)
end
