(** Character Parser: An indentation sensitive parser which parses streams of
    characters i.e. the token type is [char].
*)

open Fmlib_std.Interfaces

module Make (State: ANY) (Final: ANY) (Semantic: ANY):
sig

    (**
    - [State]: User state.
    - [Final]: Final result type of the parser.
    - [Semantic]: Semantic error message (triggered by [fail error])
    *)



    (** {1 Final Parser} *)

    (** A parser is a consumer of tokens. At the end of consumption there is a
        result which is either a successfully parsed structure or a syntax or
        semantic error. *)
    module Parser:
    sig
        include Character_intf.CHARACTER_PARSER
            with type token    = char
             and type final    = Final.t
             and type semantic = Semantic.t
             and type state    = State.t
        (** @inline *)

    end


    (** {1 Generic Combinators} *)

    include Interfaces.COMBINATOR
        with
            type state := State.t
            and type expect := string
            and type semantic := Semantic.t
    (** @inline *)


    (** {1 Location Combinators} *)

    include Character_intf.LOCATION_COMBINATORS
        with type 'a t := 'a t



    (** {1 Indentation Combinators} *)

    include Character_intf.INDENTATION_COMBINATORS
        with type 'a t := 'a t



    (** {1 End of Input} *)

    include Character_intf.END_OF_INPUT_COMBINATOR
        with type 'a t := 'a t



    (** {1 Lexer Support} *)

    include Character_intf.LEXER_COMBINATOR
        with type 'a t := 'a t



    (** {1 Character Combinators} *)

    include Character_intf.CHARACTER_COMBINATORS
        with type 'a t := 'a t





    (** {1 Parse Base 64 Encodings} *)


    include Character_intf.BASE_64_COMBINATORS with type 'a t := 'a t
    (** @inline *)






    (** {1 Make the Final Parser} *)

    include Character_intf.MAKE_FINAL_COMBINATORS
        with type 'a t   := 'a t
         and type state  := State.t
         and type final  := Final.t
         and type parser := Parser.t
end
