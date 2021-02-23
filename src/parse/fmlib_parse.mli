(** Parsing Library *)



(** {1 Introduction} *)

(**

   The parsers of this library implement parsers for {i Parsing Expression
   Grammars} with parsing combinators. They have the following main features:

   - Like all combinator parsers (e.g. like [parsec] of Haskell) you have the
   full flexibility of a functional language. There is no preprocessing step
   where the parser has to be generated from the grammar.

   - All parsers are incremental and work in push mode. You can parse part of
   the input stream, look into the state of the parser. Store the parser at
   different locations and resume parsing at any location in the input stream.

   - The character parser (i.e. the parser which works on streams of characters
   like files) parses indentation sensitive grammars.

   These features in combination are to the best of our knowledge unique in
   [Fmlib].



   Parsing expression grammars look are very similar to context free grammers.
   There are two main differences:

   - The choice between alternatives is biased. I.e. if parsing of the first
   alternative succeeds, then the next one is not parsed.

   - Left recursion is forbidden.

   Accepting these restrictions leads to fairly efficient parser which can be
   implemented directly in a functional language.

   A parsing expression in a parsing expression grammar is one of:

   - A token

   - A sequence of expressions [e₁ e₂ ...]

   - A biased choice between expressions [e₁ / e₂ / ... ]

   - Zero or more of an expression [e+]

   - One or more of an expression [e*]

*)




(** {1 Construction of Parsers} *)

(**

    You build a parser by first using combinators to express your grammar. A
    combinator has type ['a t] which parses some part of the input stream and
    returns an object of type ['a] in case of success.

    All parsers have some elementary combinators which parse one token and
    succeed or fail. E.g. the character parser has a combinator [char c] which
    succeeds, if the next character is [c] and fails otherwise. Or there is the
    combinator [charp p] where [p] is a function of type [char -> bool]. This
    combinator succeeds if the next character satisfies [p] and fails otherwise.

    As the name implies, combinators can be combined to form more complex
    combinators. The [let*] combinator allows to form sequences of grammar
    constructs. E.g. if we have some combinators [p], [q] and [r] which parse
    certain grammar constructs, we can combine them sequentially by

    {[
        let* x = p in
        let* y = q in
        let* z = r in
        return (f x y z)
    ]}

    This combinator first parses [p] which returns in case of success some value
    [x]. Then it parses [q] which returns a [y]. Then it parses [r] which
    returns a [z]. At the end it computes the compound result from the
    individual results [x], [y] and [z].

    The sequence combinator fails, if one of its components fail and it succeeds,
    if all components succeed.

    Needless to say that the combinators [p], [q] and [r] can receive arguments
    and [q] and [r] can use [x] and [r] can use [x] and [y].

    For the biased choice there is the operator [</>]. Having three combinators
    [p], [q] and [r] of the same type the expression

    {[
        p </> q </> r
    ]}

    is a new combinator which first parses [p]. In case [p] fails without
    consuming token, the next combinator [q] is used and so on.

    There is the combinator [backtrackable p] which let [p] fail without
    consuming input even if it fails after consuming some token.

    More detailed combinators can be seen in the APIs of the different parsers.

    In order to do the actual parsing we need some combinator [c] which has type
    [final t] where [final] is the desired result after parsing successfully the
    complete input stream. From this combinator we can create a complete parser
    by

    {[make initial_state c]}

    This object is the parser object. If you don't need a user state, then the
    initial state is just unit [()]. You can push token into the parser object
    by

    {[
        put token p
    ]}

    which returns a parser object into which step by step all token can be
    pushed. At the end of input you call

    {[
        put_end p
    ]}

    which terminates the parse. There are methods like

    {[
        needs_more p        (* Does the parser need more input? *)
        has_ended p         (* Has the parser finished parsing? *)
        has_succeeded p     (* Has the parser finished successfully? *)

        final p             (* The final result after successful parsing. *)
        ...
    ]}

*)



(** {1 Calculator Example} *)

(**

    In the following we define a parser which parses and evaluates arithmetic
    expressions with addition, substraction, multiplication and division. Only
    division can trigger the semantic failure {i division by zero}.

    We want to parse a stream of characters.

    {[
        module Semantic = struct
            type t = Pretty.Print.doc
        end

        module CP =
            Parse.Character.Make
                (Unit)                  (* No state needed. *)
                (Int)                   (* The parser returns a number. *)
                (String)                (* The possible semantic error. *)
        open CP
    ]}


    Some combinators to parse whitespace, arithmetic operators and numbers.

    {[
        let whitespace: int t =
            skip_zero_or_more (char ' ')

        type addop = Plus  | Minus
        type mulop = Times | Divide

        let operator (c: char) (op: 'a): 'a t =
            map (fun _ -> op) (char c)

        let addop: addop t =
            (* Parse an addition operator. *)
            let* op = operator '+' Plus </> operator '-' Minus in
            let* _  = whitespace in      (* strip whitespace *)
            return op

        let mulop: mulop t =
            (* Parse a multiplication operator. *)
            let* op = operator '*' Times </> operator '/' Divide in
            let* _  = whitespace in      (* strip whitespace *)
            return op

        let number: int t =
            (* Parse one number. *)
            let* v =
                one_or_more
                    (fun d -> d)
                    (fun v d -> 10 * v + d)
                    digit
            in
            let* _ = whitespace in      (* strip whitespace *)
            return v
    ]}


    The high precedence operations are multiplication and division. For the
    division operation we have to handle division by zero.

    {[
        let rec factors (opnd1: int): int t =
            (* Parse the factors of a product. *)
            (
                let* op    = mulop in
                let* opnd2 = number
                in
                match op with
                | Times ->
                    factors (opnd1 * opnd2)
                | Divide ->
                    if opnd2 = 0 then
                        fail "division by zero"
                    else
                        factors (opnd1 / opnd2)
            )
            </>
            return opnd1

        let product: int t =
            (* Parse a product [f1 * f2 / f3 ...]. *)
            let* n = number in
            factors n
    ]}

    Note that the recursion in [factors] is guarded. A recursiv call happens
    only if there is a multiplication operator and an operand.


    In order to parse sums we can directly use the combinator
    [one_or_more_separated].


    {[
        let expr: int t =
            (* Parse a sum [a + b - c ...]. *)
            one_or_more_separated
               (fun x -> x)
               (fun s op x ->
                    match op with
                    | Plus ->
                        s + x
                    | Minus ->
                        s - x)
               product
               addop
    ]}


    As a last step we convert the combinator [expr] to a parser by

    {[
        let calculator: Parser.t =
            make () expr
    ]}

    and run it on a string

    {[
        let p = Parser.run_on_string "1 + 2 * 6 / 2" calculator

        assert (Parser.has_succeeded p);

        assert (Parser.final p = 7);


        let p = Parser.run_on_string "1 / 0" calculator

        assert (Parser.has_failed_semantic p)
    ]}


*)




(** {1 Avoid Infinite Recursion} *)

(**

    It is easy to generate a parser which enters an infinite loop. This has to
    be avoided. {e General rule:} Each recursive call must be protected by some
    combinator which consume token before the recursive call happens.

    In order to demonstrate valid recursion we use the calculator example of the
    previous chapter and add parenthesized expressions.

    {[
        let parenthesized (p: unit -> 'a t): 'a t =
            let* _ = char '(' in
            let* _ = whitespace in
            let* x = p () in
            let* _ = char ')' in
            let* _ = whitespace in
            return x
    ]}

    This combinator parses the combinator [p] between parentheses. Instead of
    using an argument of type ['a t] we use an argument of type [unit -> 'a t]
    because the combinator [p] is usually called recursively.

    The body of [parenthesized] protects a possible recursive call of [p ()] by
    prefixing it by the combinator [char '('] which certainly consumes a token
    in case of success.

    As opposed to Haskell, Ocaml is a strict language. In Haskell this
    particular problem of infinite recursion does not occur, because the Haskell
    compiler treats internally every expression as a thunk (i.e. of type [unit
    -> e] instead of [e]) and evaluates the thunk only if needed.

    Now we can generate a combinator for parenthesized expressions by some
    mutually recursive functions.

    {[
        let rec expr (): int t =
            ... (* as above using "product ()" instead of "product" *)

        and atomic (): int t =
            number </> parenthesized expr (* recursive call to "expr ()"
                                             not yet done! *)

        and product (): int t =
            let* n = atomic () in
            factors n

        and factors (opnd1: int): int t =
            let* op    = mulop in
            let* opnd2 = atomic () in
            ... (* same as above *)
    ]}

    Note that Ocaml does not allow recursive constants. Therefore all elements
    of a set of mutually recursive functions must be {e functions}. Therefore we
    added a unit argument to combinators which do not have other arguments.

*)




(** {1 Backtracking} *)


(**
    Nearly all combinators do not backtrack. I.e. all choices are done by
    looking only at the first token of a construct. If a construct fails after
    consuming token, no alternative will be checked.

    This make parsing fast. However sometimes it is necessary to backtrack a
    failed combinator as if it had not consumed any token. After backtracking,
    any alternative combinator can be tried. This makes parsing more expensive,
    because the consumed token have to be pushed back to the lookahead and
    reparsed by the alternative combinators.

    There are three basic backtracking combinators.

    - [backtrack p expect]: Parse [p]. In case of failure push all consumed
    token back to the lookahead and continue with possible other choices. Push
    the expectation [expect] to the failed expectations.

    - [followed_by p expect]: Parse [p] and push all cosumed token back to the
    lookahead. Succeed, if [p] succeeded and fail, if [p] failed.

    - [not_followed_by p expect]: Parse [p] and push all cosumed token back to the
    lookahead. Succeed, if [p] failed and fail, if [p] succeeded.

*)





(** {1 Indentation Parsing} *)

(**

    With the character parser we can parse indented and aligned structures. Each
    construct gets a set of allowed indentations. The set has a lower
    indentation bound and an upper indentation bound. The upper indentation
    bound can be infinite.

    Initially the lower bound of the indentation set is zero and the upper bound
    is infinite (i.e. the indentation set is [{0, 1, ... }])

    No token of a construct is located to the left of the upper indentation
    bound. If the upper bound is infinite, then token have not yet been
    encountered. During parsing of a construct token are encountered and its
    indentation set is finite.

    The default case is that any construct inherits its indentation set from its
    parent.
*)



(**

    With the combinator

    {[indent i p]}

    we can indent the construct parsed by [p] by at least [i] columns
    relative to its parent.
*)

(**

    To parse aligned constructs we need a parent construct which contains the
    aligned children. The only purpose of the parent construct is to contain its
    aligned children. Usually the parent construct is indented (maybe by zero)
    relative to its parent. E.g. if we want to parse the combinators [p] and [q]
    aligned, we do this by

    {[

        let parent =
            let* a = align p in
            let* b = align q in
            return (a,b)
        in
        indent 1 (align parent)
    ]}

    In that case [parent] is the parent construct which is indented relative to
    its parent and contains the two aligned children [p] and [q].

    The combinator [align] aligns within the allowed indentations and the
    combinator [left_align] aligns leftmost within the allowed indentations.
*)


(**

    It is {e important} to exclude whitespace from all alignment and indentation
    requirements. Each line might start with blanks which can violate the
    requirements. In order to exclude constructs from the alignment and
    indentation requirements we can use the combinator

    {[
        detach p
    ]}

    which parses [p] without any indentation and alignment requirements.
*)






(** {1 API} *)

module Character = Character

module Generic = Generic

module Position = Position

module Located  = Located

module Indent = Indent

module Interfaces = Interfaces
