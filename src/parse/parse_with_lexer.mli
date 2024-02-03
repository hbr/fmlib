(** A parser which works with two components: A lexer which splits up the input
    into a sequence of tokens and parser which parses the tokens.

    The parser needs two components, a lexer and a parser. The lexer works on
    streams of characters and produces tokens of type
    [Position.range * Token.t]. The parser consumes tokens of type
    [Position.range * Token.t] and produces the parsed constructs in case of
    success.
*)



module type ANY = Fmlib_std.Interfaces.ANY


(** Generate a parser with a lexer and a token parser.

    The generated parser parses a stream of characters. The lexer is used to
    convert the stream of characters into a stream of tokens of type
    [Position.range * Token.t] which are fed into the token parser.
*)
module Make
        (State: ANY)
        (Token: ANY)
        (Final: ANY)
        (Semantic: ANY)
        (Lex: Interfaces.LEXER with type final = Position.range * Token.t
                                and type token = char)
        (Parse: Interfaces.FULL_PARSER with
                type state = State.t
            and type token = Position.range * Token.t
            and type expect= string * Indent.expectation option
            and type final = Final.t
            and type semantic = Semantic.t):
sig
    (** The type of tokens is char.
        {[
            type token = char
        ]}

        Type of syntax expectations:
        {[
            type expect = string * Indent.expectation option
        ]}
    *)


    include Parse_with_lexer_intf.PARSE_WITH_LEXER
        with type token = char
        and  type final = Final.t
        and  type expect = string * Indent.expectation option
        and  type semantic = Semantic.t
        and  type state = State.t
        and  type lexer := Lex.t
        and  type parse := Parse.t
    (* * @inline *)
end



(** Generate a parser with a utf8 lexer and a token parser.

    The generated parser parses a stream of unicode characters encoded in utf-8.
    The lexer is used to convert the stream of characters into a stream of
    tokens of type [Position.range * Token.t] which are fed into the token
    parser.
*)
module Make_utf8
        (State: ANY)
        (Token: ANY)
        (Final: ANY)
        (Semantic: ANY)
        (Lex: Interfaces.LEXER with type final = Position.range * Token.t
                                and type token = Utf8.Decoder.t)
        (Parse: Interfaces.FULL_PARSER with
                type state = State.t
            and type token = Position.range * Token.t
            and type expect= string * Indent.expectation option
            and type final = Final.t
            and type semantic = Semantic.t):
sig
    (** The type of tokens is utf-8 decoded unicode characters.
        {[
            type token = Utf8.Decoder.t
        ]}

        Type of syntax expectations:
        {[
            type expect = string * Indent.expectation option
        ]}
    *)


    include Parse_with_lexer_intf.PARSE_WITH_LEXER
        with type token = Utf8.Decoder.t
        and  type final = Final.t
        and  type expect = string * Indent.expectation option
        and  type semantic = Semantic.t
        and  type state = State.t
        and  type lexer := Lex.t
        and  type parse := Parse.t
    (* * @inline *)
end
