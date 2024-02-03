module type UNICODE_COMBINATORS =
sig
    type _ t

    val uchar: Uchar.t -> Uchar.t t
    (** [uchar uc] Parse the unicode character [uc]. *)



    val ucharp: (Uchar.t -> bool) -> string -> Uchar.t t
    (** [ucharp p error]

        Parse a unicode character which satisfies the predicate [p]. If the next
        character does not satisfy the predicate, then use the string [error]
        to express the failed expectation.
    *)


    val urange: Uchar.t -> Uchar.t -> Uchar.t t
    (** [urange uc1 uc2]

        Parse a unicode character whose scalar value is in the range between the
        scalar values of [uc1] and [uc2] including the boundaries.
    *)


    val uword: (Uchar.t -> bool) -> (Uchar.t -> bool) -> string -> string t
    (** [uword first inner error]

        Parse a sequence of unicode characters whose first character satisfies
        the predicate [first] and all subsequence characters satisfy the
        predicate [inner]. If no such word is encountered then use the string
        [error] to express the expectation.
    *)

end





module type UC =
sig
    type token
    type final
    type state
    type semantic

    (** {1 Final Parser} *)

    module Parser:
    sig
        include Character_intf.CHARACTER_PARSER
            with type token = token
             and type final = final
             and type state = state
             and type semantic = semantic
        (** @inline *)
    end


    (** {1 Generic Combinators} *)

    include Interfaces.COMBINATOR
        with type state    := state
         and type semantic := semantic
         and type expect   := string
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



    (** {1 Unicode Combinators} *)

    include UNICODE_COMBINATORS
        with type 'a t := 'a t




    (** {1 Make the Final Parser} *)

    include Character_intf.MAKE_FINAL_COMBINATORS
        with type 'a t   := 'a t
         and type state  := state
         and type final  := final
         and type parser := Parser.t
end
