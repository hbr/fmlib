module type PARSER =
sig

    (**  *)

    (**
        A parser [p] is a sink of token. As long as it signals [needs_more p]
        more token can be pushed into the parser via [put token p] or the input
        stream can be ended via [put_end p].

        If the parser does not need more token ([has_ended p] is equivalent to
        [not (needs_more p)]), then it has either succeeded or failed.

        If it has succeeded the final value is available via [final p].

        There are two types of failure:

        - Syntax error: In that case [failed_expectations p] returns the list of
        failed expectations.

        - Semantic error: In that case [failed_semantic p] returns the
        encountered semantic error.


       The function [state] returns the user state.

       The function [lookaheads] returns a pair. The first part of the pair is
       an array of unprocessed lookahead token and the second part is a flag
       indicating if the endtoken has been received via [put_end].
    *)

    type token
    (** Token type. *)


    type state
    (** User state. *)


    type final
    (** Type of the final object constructed in case of success. *)


    type expect
    (** Type of a failed expectation. *)


    type semantic
    (** Type a semantic error. *)


    type t
    (** Type of the final parser. *)


    val needs_more: t -> bool
    (** [needs_more p] Does the parser [p] need more token? *)


    val has_ended: t -> bool
    (** [has_ended p] Has the parser [p] ended parsing and either succeeded or
        failed?

        [has_ended p] is the same as [not (needs_more p)]
    *)


    val put: token -> t -> t
    (** [put token p] Push [token] into the parser [p].

        Even if the parser has ended, more token can be pushed into the parser.
        The parser stores the token as lookahead token.
    *)


    val put_end: t -> t
    (** [put_end p] Push and end token into the parser [p]. *)


    val has_succeeded: t -> bool
    (** [has_succeeded p] Has the parser [p] succeeded? *)

    val has_failed_syntax: t -> bool
    (** [has_failed_syntax p] Has the parser [p] failed with a syntax error? *)

    val has_failed_semantic: t -> bool
    (** [has_failed_semantic p] Has the parser [p] failed with a semantic error?
    *)


    val final: t -> final
    (** [final p] The final object constructed by the parser [p] in case of
        success.

        Precondition: [has_succeeded p]
    *)


    val failed_expectations: t -> expect list
    (** [failed_expectations p] The failed expectations due to a syntax error.

        Precondition: [has_failed_syntax p]
    *)


    val failed_semantic: t -> semantic
    (** [failed_semantic p] The failed semantic error.

        Precondition: [has_failed_semantic p]
    *)

    val state: t -> state
    (** [state p] The user state of the parser [p].

        Can be called at any time.
    *)

    val lookaheads: t -> token array * bool
    (** [lookaheads p] The lookahead token and and end flag of the parser [p].

        The end flag indicates, if the end token has already been received via
        [put_end p].
    *)
end



module type COMBINATOR =
sig
    (** *)


    type state
    type expect
    type semantic

    (** {2 Basic Combinators} *)

    type _ t
    (** ['a t] Type of a parse combinator returning an ['a]. *)



    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    (** [p >>= f]

        Parse first the input according to the combinator [p]. In case of
        success, feed the returned value of [p] into the function [f] to get the
        combinator to parse next.
    *)


    val (let* ): 'a t -> ('a -> 'b t) -> 'b t
    (** [let* x = p in f x] is equivalent to [p >>= f]

        The [let*] combinator let us express parsing sequences conveniently.
        Example:

        {[
            let* x = p in       (* parse [p], result [x] in case of success. *)
            let* y = q x in     (* parse [q x], result [y] ... *)
            let* z = r x y in   (* ... *)
            ...
            return f x y z ...
        ]}

        The wildcard [let* _ = ...] can be used to ignore results of
        intermediate parsing steps.
    *)


    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f p]

        Try combinator [p]. In case of success, map the returned value [x] to [f
        x]. In case of failure, do nothing.

        [map f p] is equivalent to [let* x = p in return (f x)].
    *)


    val succeed: 'a -> 'a t
    (** [succeed a]

        Succeed immediately without consuming token. Return object [a] as
        result.
    *)


    val return:  'a -> 'a t
    (** [return a] is equivalent to [succeed a]. *)


    val unexpected: expect -> 'a t
    (** [unexpected expect] triggers a syntax error signalling the expectation
        [expect]. *)


    val clear_last_expectation: 'a -> 'a t
    (** [clear_last_expectation p] Clear last failed expectation.

        This is useful e.g. after stripping whitespace. Since stripping
        whitespace means [skip_one_or_more ws] or [skip_zero_or_more ws], after
        skipping whitespace the parser can still expect more whitespace.
        Therefore there is a failed expectation *whitespace* on the stack.
        However you rarely want this expectation to be reported.
    *)


    val fail: semantic -> 'a t
    (** [fail error] triggers a semantic error. *)


    val (</>):   'a t -> 'a t -> 'a t
    (** [p </> q]

        Try first combinator [p]. In case of success or failure with consumed
        token, [p </> q] is equivalent to [p].

        If [p] fails without consuming token, then [p </> q] is equivalent to
        [q].
    *)


    val choices: 'a t -> 'a t list -> 'a t
    (** [choices p [q r t ...]] is equivalent to [p </> q </> r </> t </> ...].
     *)



    val (<?>):   'a t -> expect -> 'a t
    (** [p <?> expect]

        Try combinator [p]. In case of success or failure with consumed token,
        [p <?> expect] is equivalent to [p].

        If [p] fails without consuming token, then the failed expectations are
        replaced with the failed expectation [expect].

        Usually [p] is a combinator implementing a choice between various
        alternatives of a grammar construct. The [<?>] combinator allows to
        replace the set of failed grammar alternatives with a higher abstraction
        of the failed expectation.  E.g. instead of getting the failed
        expectations [identifier], ['('], [-], ...  we can get the failed
        expectation [expression].
    *)




    (** {2 State Combinators} *)


    val get: state t
    (** Get the current user state. *)


    val update: (state -> state) -> unit t
    (** [update f] Update the user state using [f]. *)


    val get_and_update: (state -> state) -> state t
    (** [get_and_update f] Get the current user state and then update the user
        state. The returned value is the old state. *)



    (** {2 Convenience Combinators} *)


    val optional: 'a t -> 'a option t
    (** [optional p]

        Try combinator [p].

        - Success: Return [Some a] where [a] is the returned value.
        - Failure without consuming token: Return [None]
        - Failure with consuming token: Remain in the error state.
    *)


    val zero_or_more: 'r -> ('item -> 'r -> 'r) -> 'item t -> 'r t
    (** [zero_or_more start f p]

        Try the combinator [p] as often as possible. Return [start] if [p] fails
        without consuming token. As long as [p] succeeds use [f] to accumulate
        the results.

        The first time [p] fails without consuming token, return the accumulated
        result.

        If [p] fails by consuming token, then [zero_or_more f p] fails with the
        same error.
    *)



    val one_or_more:
        ('item -> 'r)
        -> ('item -> 'r -> 'r)
        -> 'item t
        -> 'r t
    (** [one_or_more first next p]

        [one_or_more first next p] is equivalent to

        {[
            let* x = p in
            zero_or_more (first x) next p
        ]}

    *)

    val list_zero_or_more: 'a t -> 'a list t
    (** [list_zero_or_more p] Parse zero or more occurrences of [p] and returned
        the collected result in a list. *)


    val list_one_or_more:  'a t -> ('a * 'a list) t
    (** [list_zero_or_more p] Parse one or more occurrences of [p] and returned
        the collected results as a pair of the first value and a list of the
        remaining values. *)


    val skip_zero_or_more: 'a t -> int t
    (** [skip_zero_or_more p] Parse zero or more occurrences of [p], ignore the
        result and return the number of occurrences. *)


    val skip_one_or_more:  'a t -> int t
    (** [skip_one_or_more p] Parse one or more occurrences of [p], ignore the
        result and return the number of occurrences. *)


    val one_or_more_separated:
        ('item -> 'r)
        -> ('r -> 'sep -> 'item -> 'r)
        -> 'item t
        -> 'sep t
        -> 'r t
    (** [one_or_more_separated first next p sep]

        Parse one or more occurrences of [p] separated by [sep]. Use [first] to
        convert the first occurrence of [p] into the result and use [next] to
        accumulate the results.
    *)




    (** {2 Backtracking} *)


    val backtrack: 'a t -> expect -> 'a t
    (** [backtrack p expect]

        Try the combinator [p]. In case of failure with consuming token, push
        the consumed token back to the lookahead and let it fail without
        consuming token. Use [expect] to record the failed expectation.

        Backtracking reduces the performance, because the token pushed back to
        the lookahead have to be parsed again. Try to avoid backtracking
        whenever possible.
    *)


    val followed_by: 'a t -> expect -> 'a t
    (** [followed_by p expect]

        Parses [p] and backtracks (i.e. all tokens of [p] will be pushed back to
        the lookahead). In case [p] succeeds, the [followed_by] parser
        succeeds without consuming token. Otherwise it fails without consuming
        tokens.
    *)

    val not_followed_by: 'a t -> expect -> unit t
    (** [not_followed_by p expect]

        Parses [p] and backtracks (i.e. all tokens of [p] will be pushed back to
        the lookahead). In case [p] succeeds, the [not_followed_by] parser
        fails without consuming token. Otherwise it succeeds without consuming
        tokens.
    *)

    (** [followed_by] and [not_followed_by] can be used to peek into the token
        stream without consuming token. *)
end
