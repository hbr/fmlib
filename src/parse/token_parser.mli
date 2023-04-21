(** Token Parser: A parser which parses streams of user
    supplied tokens.

    It supports layout parsing and friendly error messages.

    The token parser is a functor which needs a module [Token] to describe the
    type of tokens. The generated parser receives tokens of type
    [Position.range * Token.t].
    I.e. the lexer has to parse the tokens which are usually a token
    type (identifier, number, keyword, ...) and a string representing the actual
    token. Furthermore the lexer has to provide position information of the
    location of the token in the source file.

    The combinators in a token parser work with tokens of type [Token.t].

    The generated parser works with tokens of type [Position.range * Token.t].
*)


open Fmlib_std.Interfaces

module Make
        (State: ANY)
        (Token: ANY)
        (Final: ANY)
        (Semantic: ANY):
sig
    (**
       - [State]: User state.
       - [Token]: Token. A token usually consists of a token type and a string
       represening the actual token. The generated parser receives an actual
       token together with information about the location in the file.
       - [Final]: Final result of a successful parse.
       - [Syntax]: Represents what has been syntactically expected and has not
                    been received.
       - [Semantic]: Semantic error message (triggered by [fail]).
    *)


    module Parser:
        Interfaces.FULL_PARSER
        with type state = State.t
         and type token = Position.range * Token.t
         and type expect = string * Indent.expectation option
         and type final  = Final.t
         and type semantic = Semantic.t

    include Interfaces.COMBINATOR
        with
            type state = State.t
         and type expect = String.t
         and type semantic = Semantic.t
    (** @inline *)


    (** {1 Elementary Parsing Step} *)

    val step:
        String.t
        -> (State.t -> Position.range -> Token.t -> ('a * State.t) option)
        -> 'a t
    (** [step expect f]

        Elementary parsing step.

        [expect] describes the expectation in case the parsing step fails.

        Failure can happen in 3 cases:

        - The end of input has been reached.
        - The next token is not indented properly.
        - The function [f] indicates an unexpected token by returning [None].

        The function [f] is called with two arguments.

        - The current state.
        - The start and end position of the next token.
        - The next token.

        [f] has to return an object of type ['a] and a new state, if it accepts
        the token. Or it returns the expectation, if the current token does not
        satisfy its expectation.
    *)


    (** {1 End of Input} *)

    val expect_end: 'a -> 'a t
    (** [expect_end a]

        Parse the end of input and return an [a] in case of success.

        If [p] is a parser which parses some construct then [p >>= expect_end]
        succeeds if [p] succeeds and is immediately followed by the end of
        input.
    *)


    (** {1 Located Constructs} *)

    val located: 'a t -> (Position.range * 'a) t
    (** [located p] Parse [p] and return the parse value together with its
        location in the file.
    *)


    (** {1 Indentation and Alignment} *)

    val indent: int -> 'a t -> 'a t
    (** [indent i p]

        Parse [p] indented at least [i] columns relative to its parent.

        Precondition: [0 <= i]

        The indentation of [p] is defined by the indentation of its leftmost
        token.  The leftmost token has to be indented at least [i] columns
        relative to the parent of [p].
    *)


    val align: 'a t -> 'a t
    (** [align p]

        Use the start position of the first token of [p] to align it with other
        constructs.

        In an aligned construct the first token is the leftmost token.

        Alignment makes sense if there are at least two combinators which
        are aligned and indented. E.g. suppose there are two combinators [p] and
        [q]. Then we can form
        {[
        indent 1 (
            align (
                let* a = align p in
                let* b = align q in
                return (a,b)
        ))
        ]}
        This combinator parses [p] whose first token has to be indented at least
        one column relative to its parent. And then it parses [q] whose first
        token must be aligned with the first token of [p].

        The indentation decouples the alignment of [p] and [q] with other
        aligned siblings or parents. [indent 0 ...] can be used to make the
        indentation optional.
    *)


    val left_align: 'a t -> 'a t
    (** Like {!align} but the leftmost token have to be at the lowest allowed
        column.

        If a whole sequence of aligned constructs have to be left aligned, then
        at least the first item of the sequence has to be left aligned.
    *)

    val detach: 'a t -> 'a t
    (** [detach p] Parse [p] as if there were no indentation and alignment
        requirements. *)


    (** {1 Make the Parser} *)

    val make: State.t -> Final.t t -> Parser.t
    (** [make s p] Make the parser from the combinator [p] and start it in state
        [s].
    *)
end
