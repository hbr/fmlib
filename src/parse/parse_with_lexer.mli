(** A parser which works with two components: A lexer which splits up the input
    into a sequence of tokens and parser which parses the tokens.


*)



module type ANY = Fmlib_std.Interfaces.ANY

(** Generate the parser with a lexer and a token parser.

    The generated parser parses a stream of characters. The lexer is used to
    convert the stream of characters into a stream of tokens which are fed into
    the token parser.
*)
module Make
        (State: ANY)
        (Token: ANY)
        (Final: ANY)
        (Semantic: ANY)
        (Lex: Interfaces.LEXER with type final = Position.range * Token.t)
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


    include Interfaces.NORMAL_PARSER
        with type token = char
        and  type final = Final.t
        and  type expect = string * Indent.expectation option
        and  type semantic = Semantic.t
        and  type state = State.t
    (* * @inline *)

    (** {1 Lexer and Parser} *)

    val make: Lex.t -> Parse.t -> t
    (** [make lex parse] Make the parser from a lexer and a parser. *)

    val lex: t -> Lex.t
    (** The lexer part of the parser. *)

    val parse: t -> Parse.t
    (** The parser part of the parser. *)


    (** {1 Position} *)

    val position: t -> Position.t
    (** The current position in the input. *)



    (** {1 Run on a String} *)

    val run_on_string: string -> t -> t
    (** [run_on_string str p] Run the parser [p] on the string [str]. *)
end
