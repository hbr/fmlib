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
    end





    (** {1 Generic Combinators} *)

    include Interfaces.COMBINATOR
        with
            type state = State.t
            and type expect = Expect.t
            and type semantic = Semantic.t




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





    (** {1 Make the Final Parser} *)

    val make: State.t -> Final.t t -> (State.t -> Expect.t) -> Parser.t
    (** [make state p e] Makes a parser.

        - [state] Initial state
        - [p] Combinator which returns in case of success an object of type
        [Final.t]
        - [e] Error function. Generates an expectation from the state. The
        function is used if at the expected end of input other token arrive.

        The generated parser expects a token stream which can be successfully
        parsed by the combinator [p] and then it expects the end of input.
    *)

    val make_parser: State.t -> Final.t t -> Parser.t
end
