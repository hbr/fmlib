(** A Generic Parser where all parameters are customizable. *)


open Fmlib_std.Interfaces


module Make
        (Token:    ANY)
        (State:    ANY)
        (Expect:   ANY)
        (Semantic: ANY)
        (Final:    ANY):
sig
    (**
        - [Token.t] Token type
        - [State.t] Type of the user state
        - [Expect.t] Type of syntax messages which are generated, when
        something has been expected but not found.
        - [Semantic.t] Type of semantic error messages. Triggered by [fail
        error].
        - [Final.t] Type of the returned object, when parsing has finished.
    *)




    (** {1 Final parser} *)


    (** The final parser.
    *)
    module Parser:
    sig
        include Interfaces.PARSER
            with type token    = Token.t
             and type final    = Final.t
             and type expect   = Expect.t
             and type semantic = Semantic.t
             and type state    = State.t
        (** @inline *)
    end





    (** {1 Generic Combinators} *)

    include Interfaces.COMBINATOR
        with
            type state = State.t
            and type expect = Expect.t
            and type semantic = Semantic.t
    (** @inline *)




    (** {1 Elementary Parsing Step} *)

    val step:
        (State.t -> Token.t option -> ('a * State.t, Expect.t) result)
        ->
        'a t
    (** [step f]

        Elementary parsing step.

        The function [f] is called with two arguments:
        - The current state
        - The next lookahead token (or none, if the end of the token stream has
        been reached).

        [f] must return either an object of type ['a] and a new state if it
        accepts the token, or a failed expectation if it rejects the token.
    *)


    val expect_end:
        (State.t -> Expect.t) -> 'a -> 'a t
    (** [expect_end error a] Expect the end of input.

        In case of success return [a]. In case of failure (i.e. not yet at the
        end of input) then compute via [error] the syntax error from the state.

        WARNING: This combinator only makes sense if you generate your parser
        with [make_parser]. If you generate your parser with [make] then the end
        of input is automatically expected after the toplevel construct.
    *)




    (** {1 Update Failed Expectations} *)



    val update_expectations:
        (State.t -> Token.t option -> Expect.t) -> 'a t -> 'a t



    (** {1 Make the Final Parser} *)

    val make: State.t -> Final.t t -> (State.t -> Expect.t) -> Parser.t
    (** [make state p e] Makes a parser.

        - [state] Initial state
        - [p] Combinator which returns in case of success an object of type
        [Final.t]
        - [e] Error function. Generates an expectation from the state. The
        function is used if an other token arrives at the expected end of input.

        The generated parser expects a token stream which can be successfully
        parsed by the combinator [p]. It can succeed only if an end token is
        pushed to the parser.
    *)


    val make_partial: State.t -> Final.t t -> Parser.t
    (** [make_partial state c].

        Makes a parser which starts in state [state] and parses a construct
        defined by the combinator [c]. The parser can succeed, even if no end
        token is pushed to the parser.
    *)


    val restart_partial: Final.t t -> Parser.t -> Parser.t
    (** [restart_partial c p]

        Restart the partial parser [p] by using the combinator [c] to recognize
        the next part of the input stream. The restarted parser starts with the
        state of [p].

        Preconditions:
        - [has_succeeded p]
        - [not (has_consumed_end p)]
    *)
end
