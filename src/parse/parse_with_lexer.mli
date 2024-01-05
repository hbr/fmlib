(** A parser which works with two components: A lexer which splits up the input
    into a sequence of tokens and parser which parses the tokens.

    The parser needs two components, a lexer and a parser. The lexer works on
    streams of characters and produces tokens of type
    [Position.range * Token.t]. The parser consumes tokens of type
    [Position.range * Token.t] and produces the parsed constructs in case of
    success.
*)



module type ANY = Fmlib_std.Interfaces.ANY

(** Generate the parser with a lexer and a token parser.

    The generated parser parses a stream of characters. The lexer is used to
    convert the stream of characters into a stream of tokens of type
    [Position.range * Token.t] which are fed into the token parser.
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



    (** {1 Partial Parser} *)

    (**

        If the input stream shall be parsed in parts, then a parser with a lexer
        can be used for partial parsing as well.

        Note that the lexer must be partial, because it succeeds after
        successfully parsing a lexical token from the input stream and is
        restarted afterwards. The restart of the lexer transfers the lookahead
        from the previous lexer to the next lexer.

        A parser with a lexer becomes partial, if the token parser is partial.
        As user of this module you have to transfer only the lookahead buffer
        from the old token parser to the next token parser.

        If the old and the new token parser have the same type, then the
        function {!make_next} can be used to transfer the lookahead buffer.

        If the old and the new token parser have different types then the
        following will do the job. Assume that [TP1.t] and [TP2.t] are the types
        of the old and new token parser, [P1.t] and [P2.t] are the types of the
        corresponding parsers with lexers and [tp2: TP2.t] is the new token
        parser

        {[
            assert (P1.has_succeeded p1);
            assert (not (P1. has_consumed_end p1));
            let lex = P1.lex p1
            and tp1 = P1.parse p1
            in
            let tp2 = TP2.fold_lookahead tp2 TP2.put TP2.put_end tp1 in
            let p2  = P2.make lex tp2 in
            ...
        ]}

        Note that as described in the chapter {{!page-parse_partial} Partial
        Parsing} the parser [p2] might have used the lookaheads of [p1] to
        either succeed or fail. You can continue parsing the input stream only
        of this is not yet the case. Otherwise you might need a new subsequent
        token parser to continue to parse the remaining input stream.
     *)



    val make_next: t -> Parse.t -> t
    (** [make_next p tp]

        This function assumes that [p] has been made with a partial token parser
        and has already successfully consumed a part of the input stream and
        [tp] is the token parser which shall be used to parse the next part of
        the input stream.

        Since the token parser contained in [p] might have unconsumed lookahead
        tokens, these tokens must be transferred to the new token parser [tp].

        The call [make_next p tp] makes a new parser with lexer using the old
        lexer and the new token parser [tp] with all the lookaheads transferred
        to it.
    *)




    (** {1 Position} *)

    val position: t -> Position.t
    (** The current position in the input. *)


    val range: t -> Position.range
    (** The current range in the input; usually the range of the first lookahead
        token. In case of a syntax error this is the unexpected token i.e. the
        token which caused the syntax error.
    *)




    (** {1 Run the Parser} *)

    val run_on_string: string -> t -> t
    (** [run_on_string str p] Run the parser [p] on the string [str]. *)


    val run_on_string_at: int -> string -> t -> int * t
    (** [run_on_string str start p] Run the parser [p] on the string [str]
        starting at index [start] Return the parser and the index next to be
        pushed in. *)

    val run_on_channel: Stdlib.in_channel -> t -> t
    (** [run_on_channel ch p] Run the parser [p] on the channel [ch]. *)
end
