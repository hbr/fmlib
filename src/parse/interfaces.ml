module type MINIMAL_PARSER =
sig
    type t
    type token
    type item = token

    val needs_more: t -> bool
    val put: token -> t -> t
    val put_end: t -> t

    type final
    val has_succeeded: t -> bool
    val has_consumed_end: t -> bool
    val final: t -> final

    type expect
    val has_failed_syntax: t -> bool
    val failed_expectations: t -> expect list
end

module type NORMAL_PARSER =
sig
    include MINIMAL_PARSER

    type semantic
    val has_failed_semantic: t -> bool
    val failed_semantic: t -> semantic

    type state
    val state: t -> state
end


module type FULL_PARSER =
sig
    include NORMAL_PARSER

    val first_lookahead_token: t -> token option
    val has_received_end: t -> bool
    val has_consumed_end: t -> bool
    val fold_lookahead: 'a -> (token -> 'a -> 'a) -> ('a -> 'a) -> t -> 'a
    val transfer_lookahead: t -> t -> t
end



module type LEXER =
sig
    include MINIMAL_PARSER
        with type token = char
         and type expect = string * Indent.expectation option

    val has_consumed_end: t -> bool
    val position: t -> Position.t

    val start:   t
    val restart: t -> t
end


module type PARSER =
sig
    type token
    type item = token
    type state
    type final
    type expect
    type semantic
    type t
    val needs_more: t -> bool
    val has_result: t -> bool
    val has_ended: t -> bool
    val has_received_end: t -> bool
    val has_consumed_end: t -> bool
    val put: token -> t -> t
    val put_end: t -> t
    val has_succeeded: t -> bool
    val has_failed_syntax: t -> bool
    val has_failed_semantic: t -> bool
    val final: t -> final
    val failed_expectations: t -> expect list
    val failed_semantic: t -> semantic
    val state: t -> state
    val has_lookahead: t -> bool
    val first_lookahead_token: t -> token option
    val fold_lookahead: 'a -> (token -> 'a -> 'a) -> ('a -> 'a) -> t -> 'a
    val transfer_lookahead: t -> t -> t
    val lookaheads: t -> token array * bool
end




module type COMBINATOR =
sig
    type state
    type expect
    type semantic

    type _ t
    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    val (let* ): 'a t -> ('a -> 'b t) -> 'b t
    val map: ('a -> 'b) -> 'a t -> 'b t
    val map_and_update: (state -> 'a -> 'b * state) -> 'a t -> 'b t
    val succeed: 'a -> 'a t
    val return:  'a -> 'a t
    val unexpected: expect -> 'a t
    val clear_last_expectation: 'a -> 'a t
    val fail: semantic -> 'a t
    val (</>):   'a t -> 'a t -> 'a t
    val choices: 'a t -> 'a t list -> 'a t
    val (<?>):   'a t -> expect -> 'a t
    val no_expectations: 'a t -> 'a t

    val get: state t
    val set: state -> unit t
    val update: (state -> state) -> unit t
    val get_and_update: (state -> state) -> state t
    val state_around:
        (state -> state) -> 'a t -> (state -> 'a -> state -> state) -> 'a t



    val optional: 'a t -> 'a option t

    val zero_or_more_fold_left:
        'r -> ('r -> 'a -> 'r t) -> 'a t -> 'r t

    val one_or_more_fold_left:
        ('a -> 'r t) -> ('r -> 'a -> 'r t) -> 'a t -> 'r t


    val zero_or_more: 'a t -> 'a list t

    val one_or_more:  'a t -> ('a * 'a list) t

    val skip_zero_or_more: 'a t -> int t

    val skip_one_or_more:  'a t -> int t


    val one_or_more_separated:
        ('item -> 'r t)
        -> ('r -> 'sep -> 'item -> 'r t)
        -> 'item t
        -> 'sep t
        -> 'r t

    val parenthesized:
        ('lpar -> 'a -> 'rpar -> 'b t)
        -> 'lpar t
        -> (unit -> 'a t)
        -> ('lpar -> 'rpar t)
        -> 'b t

    val operator_expression:
        'exp t
        -> 'op t option
        -> 'op t
        -> ('op -> 'op -> bool t)
        -> ('op -> 'exp -> 'exp t)
        -> ('exp -> 'op -> 'exp -> 'exp t)
        -> 'exp t

    val backtrack: 'a t -> expect -> 'a t

    val followed_by: 'a t -> expect -> 'a t

    val not_followed_by: 'a t -> expect -> unit t
end
