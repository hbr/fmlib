(** Module types *)



(** A minimal parser is a sink of tokens which either succeeds or returns a list
    of failed syntax expectations.

*)
module type MINIMAL_PARSER =
sig
    (** *)
    (**
        A parser [p] is a sink of token. As long as it signals [needs_more p]
        more token can be pushed into the parser via [put token p] or the input
        stream can be ended via [put_end p].

        [has_ended  p] is equivalent to [not (needs_more p)]. [has_ended  p]
        signals that the parser has either succeeded or failed.

        If it has succeeded the final value is available via [final p].
    *)


    type t          (** Type of the parser. *)

    (** {1 Feeding Tokens} *)

    type token      (** Type of the tokens. *)

    type item = token
    (** In order to conform to the interface {!Fmlib_std.Interfaces.SINK}. *)

    val needs_more: t -> bool
    (** [needs_more p] Does the parser [p] need more tokens? *)

    val put: token -> t -> t
    (** [put tok p] Push token [tok] into the parser [p].

        Even if the parser has ended, more tokens can be pushed into the parser.
        The parser stores the token as lookahead token.

        If the parser has already received the end of the token stream via
        {!put_end}, then all subsequent tokens are ignored.
    *)

    val put_end: t -> t
    (** [put_end p] Push and end token into the parser [p].

    *)


    (** {1 Success} *)

    type final      (** Type of the final result. *)

    val has_succeeded: t -> bool
    (** [has_succeeded p] Has the parser [p] succeeded? *)


    val has_ended: t -> bool
    (** [has_ended p] Has the parser [p] ended parsing and either succeeded or
        failed?

        [has_ended p] is the same as [not (needs_more p)]
    *)


    val has_consumed_end: t -> bool
    (** Has the parser consumed the end of input? *)


    val final: t -> final
    (** [final p] The final object constructed by the parser [p] in case of
        success.

        Precondition: [has_succeeded p]
    *)



    (** {1 Syntax Errors} *)

    type expect     (** Type of expectations. *)

    val has_failed_syntax: t -> bool
    (** [has_failed_syntax p] Has the parser [p] failed with a syntax error? *)

    val failed_expectations: t -> expect list
    (** [failed_expectations p] The failed expectations due to a syntax error.

        Precondition: [has_failed_syntax p]
    *)
end








(** A normal parser parses a stream of tokens like a {!MINIMAL_PARSER}. In
    addition it can have a state and semantic errors.
*)
module type NORMAL_PARSER =
sig
    include MINIMAL_PARSER
    (** @inline *)

    (** {1 Semantic Errors} *)

    type semantic (** Type of semantic errors. *)

    val has_failed_semantic: t -> bool
    (** Has the parser failed because of a semantic error? *)

    val failed_semantic: t -> semantic
    (** The semantic error encountered.

        Precondition: A semantic error has occurred.
    *)


    (** {1 State} *)

    type state (** Type of the state of the parser (in many cases [unit]) *)

    val state: t -> state
    (** The state of the parser. *)
end










(** A full parser parses a stream of tokens like a {!MINIMAL_PARSER}. In
    addition it can have a state, semantic errors and gives access to the
    lookahead tokens.
*)
module type FULL_PARSER =
sig
    include NORMAL_PARSER
    (** @inline *)


    (** {1 Lookaheads} *)


    val has_lookahead: t -> bool
    (** [has_lookahead p] Are there any unconsumed lookahead tokens in the buffer
        or has the end token not yet been consumed? *)


    val first_lookahead_token: t -> token option
    (** The first lookahead token (or [None] in case there is none). *)

    val has_received_end: t -> bool
    (** [has_received_end p] Has the parser [p] already received the end of
        token stream via [put_end]?
     *)

    val has_consumed_end: t -> bool
    (** [has_consumed_end p] Has the parser [p] already received the end of
        token stream via [put_end] and consumed it?
     *)

    val fold_lookahead: 'a -> (token -> 'a -> 'a) -> ('a -> 'a) -> t -> 'a
    (** [fold_lookahead a  ftok fend p]

        Fold the lookahead tokens with the start value [a] and the folding
        function [ftok]. At the end of the lookahead tokens, call [fend] if
        there is an unconsumed end.
    *)


    val transfer_lookahead: t -> t -> t
    (** [transfer_lookahead p_old p_new]

        Transfer the lookahead tokens from [p_old] to [p_new]
    *)


    val lookaheads: t -> token array * bool
    (** [lookaheads p] The lookahead token and and end flag of the parser [p].

        The end flag indicates that the end token has already been received via
        [put_end p].
    *)
end









(** A lexer is a restartable parser where the tokens are characters.
 *)
module type LEXER =
sig
    (**

        A lexer analyses a stream of characters and groups the stream of
        characters into tokens. It usually strips off whitespace. I.e. a lexer
        expects a stream of characters of the form
        {v
            WS Token WS Token ... WS Token WS EOS
         v}
        [WS] is a possibly empty sequence of whitespace characters like
        blanks, tabs and newlines and comments. [Token] represents a legal
        token. [EOS] represents the end of the stream.


        A lexer is in one of three states:

        - {!needs_more}: The lexer needs more characters from the stream of
        characters in order to decide the next correct token or the end of
        input. The lexer is ready to receive more characters via {!put} or to
        receive the end of input via {!put_end}.

       - {!has_succeeded}: The lexer has found a correct token or detected the
       end of input. In this state (except at the end of inpute) the lexer can
       be restarted to find the next token.

       - {!has_failed_syntax}: The lexer has detected a character (or the end of
       intput) which cannot be part of a legal token.

       In the state {!has_succeeded} the lexer signals via {!has_consumed_end}
       that the end of input has been reached.

       A module conforming to the module type [LEXER] can be used in the module
       {!module:Parse_with_lexer} to create a two stage parser where the lexer
       handles tokens and a combinator parser handles the higher level
       constructs.

     *)

    include MINIMAL_PARSER
        with type expect = string * Indent.expectation option
    (** @inline *)

    (** {1 Lookahead} *)

    val has_consumed_end: t -> bool
    (** Has the lexer consumed the end of input? *)


    (** {1 Position} *)

    val position: t -> Position.t
    (** Line and column number of the current position of the lexer.

    *)

    (** {1 Start} *)

    val start: t
    (** The lexer for the first token. *)


    (** {1 Restart}

        A lexer does not consume the entire input stream. It just consumes
        characters until a token has been recognized. In case of the successful
        recognition of a token, it returns the token (see {!type-final}). Then
        it can be restarted to recognize the next token.
    *)

    val restart: t -> t
    (** [restart p]

        Next lexer, ready to recognize the next token of the input stream.

        All lookaheads from the previous lexer are pushed onto the new lexer
        which starts a the position where the previous lexer finished.

        Preconditions:
        - [has_succeeded p]
        - [not (has_consumed_end p)]
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


    val map_and_update: (state -> 'a -> 'b * state) -> 'a t -> 'b t
    (** [map_and_update f p]

        Try combinator [p]. In case of success, map the returned state [state]
        and value [a] to [f state a]. In case of failure, do nothing.

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
        [expect].
        @deprecated Don't use this function. It will be removed in future
        versions.
    *)


    val clear_last_expectation: 'a -> 'a t
    (** [clear_last_expectation p] Clear last failed expectation.

        This is useful e.g. after stripping whitespace. Since stripping
        whitespace means [skip_one_or_more ws] or [skip_zero_or_more ws], after
        skipping whitespace the parser can still expect more whitespace.
        Therefore there is a failed expectation *whitespace* on the stack.
        However you rarely want this expectation to be reported.

        @deprecated Use {!no_expectations}
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


    val no_expectations: 'a t -> 'a t
    (** [no_expectations p]

        Parse the combinator [p].

        - [p] fails: [no_expectations p] fails with the same error.

        - [p] succeeds without consuming tokens: [no_expectations p] succeeds
        without any added expectations.

        - [p] succeeds and consumes some token: [no_expectations p] succeeds
        without any expectations.

        Many combinators can succeed with expectations. E.g. the combinator
        {{!optional} [optional p]} expects a [p] and succeeds if it does not
        encounter a construct described by [p]. All repetitive combinators like
        {!one_or_more} try to consume as many items as possible. At the end they
        are still expecting an item.

        This combinator allows to clear such unneeded expectations. It is
        particularly useful when removing whitespace. The expectation of
        whitespace is not a meaningful error message to the user.
    *)


    (** {2 State Combinators} *)


    val get: state t
    (** Get the current user state. *)


    val set: state -> unit t
    (** Set the user state. *)


    val update: (state -> state) -> unit t
    (** [update f] Update the user state using [f]. *)


    val get_and_update: (state -> state) -> state t
    (** [get_and_update f] Get the current user state and then update the user
        state. The returned value is the old state. *)


    val state_around:
        (state -> state) -> 'a t -> (state -> 'a -> state -> state) -> 'a t
    (** [state_around before p after]

        If [s0] is the initial state, then execute [p] with the start state
        [before s0] and set the update the final state [s1] by [after s0 a s1]
        where [a] is the returned value in case of success and [s1] is the final
        state after executing [p].
    *)


    (** {2 Optional Elements} *)


    val optional: 'a t -> 'a option t
    (** [optional p]

        Try combinator [p].

        - Success: Return [Some a] where [a] is the returned value.
        - Failure without consuming token: Return [None]
        - Failure with consuming token: Remain in the error state.
    *)






    (** {2 Repetition} *)


    val zero_or_more_fold_left: 'r -> ('r -> 'a -> 'r t) -> 'a t -> 'r t
    (** [zero_or_more_fold_left start f p]

        Try the combinator [p] as often as possible. Accumulate the results to
        the start value [start] using the folding function [f]. The accumulation
        happens left to right. I.e. if 3 repetitions are encountered the
        folding function [f] is called
        {[
            f (f (f start a1) a2) a3
        ]}
    *)


    val zero_or_more_fold_right: ('a -> 'r -> 'r t) -> 'a t -> 'r -> 'r t
    (** [zero_or_more_fold_left f p start]

        Try the combinator [p] as often as possible. Accumulate the results to
        the start value [start] using the folding function [f]. The accumulation
        happens right to left. I.e. if 3 repetitions are encountered the
        folding function [f] is called
        {[
            f a1 (f a2 (f a3 start))
        ]}

    *)



    val one_or_more_fold_left:
        ('a -> 'r t) -> ('r -> 'a -> 'r t) -> 'a t -> 'r t
    (** [one_or_more_fold_left first f p]

        Try the combinator [p] at least once and then as often as possible. Put
        the first value returned by [p] into the function [first] returning a
        result and accumulate the subsequent values as often as possible and
        accumulate the results to the start value returned by [first] using the
        folding function [f].
    *)


    val zero_or_more: 'a t -> 'a list t
    (** [zero_or_more p] Parse zero or more occurrences of [p] and return
        the collected result in a list. *)


    val one_or_more:  'a t -> ('a * 'a list) t
    (** [zero_or_more p] Parse one or more occurrences of [p] and return
        the collected results as a pair of the first value and a list of the
        remaining values. *)


    val skip_zero_or_more: 'a t -> int t
    (** [skip_zero_or_more p] Parse zero or more occurrences of [p], ignore the
        result and return the number of occurrences. *)


    val skip_one_or_more:  'a t -> int t
    (** [skip_one_or_more p] Parse one or more occurrences of [p], ignore the
        result and return the number of occurrences. *)


    val one_or_more_separated:
        ('item -> 'r t)
        -> ('r -> 'sep -> 'item -> 'r t)
        -> 'item t
        -> 'sep t
        -> 'r t
    (** [one_or_more_separated first next p sep]

        Parse one or more occurrences of [p] separated by [sep]. Use [first] to
        convert the first occurrence of [p] into the result and use [next] to
        accumulate the results.
    *)



    val counted: int -> int -> 'r -> (int -> 'e -> 'r -> 'r) -> 'e t -> 'r t
    (** [counted min max start next p]

        Collect between [min] and [max] numbers if elements recognized by the
        combinator [p] and accumulate them with the folding function [next] into
        the start value [start].
    *)



    (** {2 Parenthesized expressions} *)

    val parenthesized:
        ('lpar -> 'a -> 'rpar -> 'b t)
        -> 'lpar t
        -> (unit -> 'a t)
        -> ('lpar -> 'rpar t)
        -> 'b t
    (** [parenthesized make lpar p rpar]

        Parse an expression recognized by the combinator [p] enclosed within
        parentheses. [lpar] recognizes the left parenthesis and [rpar]
        recognizes the right parenthesis. The value returned by [lpar] is given
        to [rpar]. With that mechanism it is possible to recognize matching
        parentheses of different kinds.

        After successful parsing the function [make] is called with the final
        value (and the parentheses).

        The combinator [p] is entered as a thunk in order to be able to call it
        recursively. In the combinator [parenthesized] the combinator [p] is
        called only guardedly. Therefore the combinator [p] can contain nested
        parenthesized expressions.

        Precondition: The combinator [lpar] has to consume at least one token in
        case of success.
    *)





    (** {2 Operator expressions} *)


    val operator_expression:
        'exp t
        -> 'op t option
        -> 'op t
        -> ('op -> 'op -> bool t)
        -> ('op -> 'exp -> 'exp t)
        -> ('exp -> 'op -> 'exp -> 'exp t)
        -> 'exp t
    (** {[
            operator_expression
                primary         (* Parse a primary expression *)
                unary_operator  (* Parse a unary operator *)
                binary_operator (* Parse a binary operator *)
                is_left         (* Is the left operator binding stronger? *)
                make_unary      (* Make a unary expression from the operator and
                                   its operand *)
                make_binary     (* Make a binary expression from the operator
                                   and its operands *)
        ]}

        Parse an operator expression by using the following combinators:

        - [is_left o1 o2] decides, if the operator [o1] on the left has more
        binding power than the operator [o2]. I.e. if the unary operator [u] has
        more binding power than the binary operator [o], then [u a o b] is
        parsed as [(u a) o b]. If the binary operator [o1] has more binding
        power than the binary operator [o2], then [a o1 b o2 b] is parsed as [(a
        o1 b) o2 c].

        - [make_unary u a] makes the unary expression [(u a)].

        - [make_binary a o b] makes the binary expression [(a o b)].

        - [primary] parses a primary expression.

        - [unary_operator] parses a unary operator.

        - [binary_operator] parses a binary operator.

        Precondition: [primary], [unary_operator] and [binary_operator] have to
        consume at least one token in case of success. Otherwise infinite
        recursion can happen.
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






(** Decoder for unicode characters. *)
module type CHAR_DECODER =
sig
    (**

       A character decoder has some valid initial value {!init}. In order to
       decode a character bytes has to be pushed into the decoder by {!put}
       until the decoder is either complete ({!is_complete}) or has encoutered
       some decoding error ({!has_error}).

       If the decoding is complete the decoded unicode character is returned by
       {!uchar}. As long as the scan is not complete or if the scan has
       encountered some decoding error the function {!uchar} returns [rep =
       U+FFFD].

    *)



    type t  (** Partially or completely scanned unicode character. *)

    val is_complete: t -> bool
    (** Has the unicode character been completely scanned? *)

    val has_error:   t -> bool
    (** Has the scanning of the unicode character encountered an encoding
        error? *)


    val uchar:       t -> Uchar.t
    (** The unicode character. In case of an error or a not yet completed scan,
        the unicode character [rep = U+FFFD] is returned.
     *)


    val scalar:      t -> int
    (** The scalar value of the unicode character. *)


    val width:       t -> int
    (** The visible width of the unicode character.
    *)


    val byte_width:  t -> int
    (** Number of bytes used during decoding. *)



    val is_newline:  t -> bool
    (** Is the decoded character a newline character?
    *)



    val init: t
    (** Initial value representing the completely scanned unicode character
       [U+0000].

        The following assertions are valid:

        - [is_complete init]
        - [not (has_error init)]
     *)



    val put: char -> t -> t
    (** [put byte dec] Feed the scanner with the next byte of the encoded
        unicode character.
     *)
end




(** Encoder for unicode characters. *)
module type CHAR_ENCODER =
sig
    type t = Uchar.t

    val to_internal: t -> string
    (** Internal representation in Ocaml is utf-8 *)

    val to_external: t -> string
    (** The external representation in the corresponding encoding. *)
end




(** Encoder and decoder for unicode characters. *)
module type CHAR_CODEC =
sig
    module Encoder: CHAR_ENCODER

    module Decoder: CHAR_DECODER
end
