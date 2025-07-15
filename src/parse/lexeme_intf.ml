module type COMBI =
sig
    (** {1 Combinators} *)

    (** Subset of the combinators of the module {!module:Character.Make}.
        Detailed description see there. *)


    type _ t

    val return: 'a -> 'a t
    val map: ('a -> 'b) -> 'a t -> 'b t
    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    val ( </> ): 'a t -> 'a t -> 'a t
    val ( <?> ): 'a t -> string -> 'a t
    val located: 'a t -> 'a Located.t t
    val unexpected: string -> 'a t
    val backtrack: 'a t -> string -> 'a t
    val char: char -> char t
    val charp: (char -> bool) -> string -> char t
    val one_of_chars: string -> string -> char t
    val string: string -> string t
    val optional: 'a t ->  'a option t
    val zero_or_more_fold_left: 'r -> ('r -> 'a -> 'r t) -> 'a t -> 'r t
    val one_or_more_fold_left:  ('a -> 'r t) -> ('r -> 'a -> 'r t) -> 'a t -> 'r t
    val skip_zero_or_more: 'a t -> int t

    val operator_expression:
        'exp t
        -> 'op t option
        -> 'op t
        -> ('op -> 'op -> bool t)
        -> ('op -> 'exp -> 'exp t)
        -> ('exp -> 'op -> 'exp -> 'exp t)
        -> 'exp t
end


(** Definition of Standard Tokens *)
module type LANG =
sig
    (** {1 Basic Definitions for a Language} *)


    val whitespace_chars: string
    (** Set of whitespace characters. A sequence of zero or more whitespace
        characters is stripped off after each token.

        Usually the set of whitespace characters is [" \t\n\r"] i.e. blanks,
        tabs, newline and carriage return are whitespace characters.
    *)


    val multiline_comment: (string * string * bool) option
    (** [multiline_start, multiline_end, nested]

        Optional definition of a multiline comment.

        Precondition: The start and end of a multiline comment must not be
        empty.
    *)


    val line_comment: string option
    (** Optional start of a line comment. The comment spans to the end of the
        line.

        Precondition: The start of a line comment must not be empty.
    *)


    val identifier_start:  char -> bool
    (** Legal characters to start an identifer. *)


    val identifier_inner: char -> bool
    (** Legal characters in an identifer after the start character. *)


    val reserved_names: string list
    (** List of identifiers which are treated as reserved names (aka keywords).

    *)
end



module type MAKE =
    functor (Combi: COMBI) (Lang: LANG) ->
    sig
        open Combi


        (** {1 Lexeme Parsers}

            In a lexeme parser all tokens strip off whitespace which come after
            the token. Therefore each combinator starts at a position in the
            input stream which does not start any whitespace.

            This is true for all tokens except the first token. Therefore the
            whitespace at the beginning of the stream has to be stripped off
            separately.

            All tokens in this module strip off the whitespace comming after it.
            If the parsing of any token fails, then an alternative combinator
            can take over at the start position of the failed token. The module
            does the necessary backtracking.

            If the user of the module adds own tokens, the token shall satisfy
            the same requirement. Strip off all whitespace after the token. In
            case of failure of a multicharacter token backtracking is done
            appropriately.
        *)




        (** {1 Basic Lexeme Support} *)


        val whitespace: int t
        (** Strip off any sequence of whitespace characters and comments. Return
            the number of characters stripped off.
         *)


        val whitespace_before: 'a t -> 'a t
        (** [whitespace_before p]

            Strip off whitespace and then continue parsing with [p].
        *)


        val lexeme: 'a t -> 'a t
        (** [lexeme p]

            Convert [p] to a lexeme parser i.e. strip off any whitespace after
            successfully parsing with [p].
        *)


        val token: string -> 'a t -> 'a t
        (** [token expect p]

            Convert a token parser [p] which does not adhere to the conventions
            of a lexeme parser (i.e. no whitespace stripped after the token, no
            backtracking in case of failure) into a parser which respects the
            conventions.

            The string [expect] describes the expected token. It might appear in
            error messages.
        *)




        (** {1 Token}

            All whitespace after tokens is stripped off. If a token fails, then
            the next alternative can be checked at the start position of the
            token. No [backtrack] is necessary.
         *)

        val semicol:  char t
        val comma:    char t
        val colon:    char t
        val dot:      char t

        val string: string -> string Located.t t
        val unsigned_int: int Located.t t
        val int: int Located.t t
        val float: float Located.t t
        val identifier: string Located.t t
        val reserved: string -> string Located.t t

        (*val char_literal: char t
          val string_literal: string t*)





        (** {1 Parenthesized Structures}

            Note that all combinators parsing the inner part of the parentheses
            have the type [unit -> 'a t]. The are called only if the opening
            parenthesis has been parsed successfully. This makes it possible to
            use the parenthesized structures recursively.
        *)


        val parens: (unit -> 'a t) -> 'a t
        val braces: (unit -> 'a t) -> 'a t
        val brackets: (unit -> 'a t) -> 'a t
        val angulars: (unit -> 'a t) -> 'a t
        (*val semicol_separated0: 'a t -> 'a list t
          val semicol_separated1: 'a t -> 'a list t
          val comma_separated0:   'a t -> 'a list t
          val comma_separated1:   'a t -> 'a list t*)



        (** {1 Operator Expressions } *)


        type assoc = Left | Right
        (** Associativity of an operator *)



        type 'e unary_operation  =
            Position.range -> 'e Located.t -> 'e t
        (** A unary operation is a function, mapping the position of the operator
            and a located operand into the result. Note that the result is not
            located. The library computes the location.
        *)

        type 'e binary_operation =
            'e Located.t -> Position.range -> 'e Located.t -> 'e t
        (** A unary operation is a function, mapping a located left operand, the
            position of the operator and a located right operand into the
            result. Note that the result is not located. The library computes
            the location.
        *)

        type 'e operation =
            | Unary  of 'e unary_operation
            | Binary of 'e binary_operation
            | Both   of 'e unary_operation * 'e binary_operation


        type 'e operator_table =
            (string * assoc * 'e operation) list list
        (** An operator table describes the precedence, associativity and the
            semantics of operators. It is a list of operators where each entry
            in the list is a list of operators at the same precedence level.

            The precedences are descending i.e. the first entry in the list are
            the operators with the highest precedence.

            The following example describes the addition, multiplication and
            exponentian operators for floating point arithmetic. The addition
            operators [+] and [-] are at the lowest precedence and the
            exponentiation operator [^] has the highest precedence.

            {[
                [
                    [ "^", Right, Binary (lift_binary ( ** ))]
                    ;
                    [ ("*", Left, Binary (lift_binary ( *. )));
                      ("/", Left, Binary (lift_binary ( /. ))) ]
                    ;
                    [ ("+", Left, Both (lift_unary (~+.), lift_binary (+.)));
                      ("-", Left, Both (lift_unary (~-.), lift_binary (-.))) ]
                ]
            ]}
        *)


        val lift_unary: ('e -> 'e) -> 'e unary_operation
        (** [lift_unary f]

            Lift the function [f] doing the unary operation into an ['e
            unary_operation] ignoring the location information.
        *)


        val lift_binary: ('e -> 'e -> 'e) -> 'e binary_operation
        (** [lift_binary f]

            Lift the function [f] doing the binary operation into an ['e
            binary_operation] ignoring the location information.
        *)


        val expression:
            string t
            -> ((unit -> 'e Located.t t) -> 'e Located.t t)
            -> 'e operator_table
            -> 'e Located.t t
        (** [expression operator primary table]

            Make a parser for operator expressions with the following arguments:

            - [operator] Parsing combinator of an operator. The combinator needs
            not respect the conventions for a lexeme parser. The function
            handles stripping off whitespace and backtracking properly.

            - [primary] Parsing combinator for primary expressions. Primary
            expressions are either tokens (numbers, variables, ...) or more
            complex expressions which are treated as atomic expressions (e.g.
            parenthesized expressions, function calls, ...). The [primary]
            combinator can use the generated expression parser recursively after
            cosuming at least one character. This convention is necessary to
            avoid unbounded recursion.

            - [table] Table describing the operators.


            Example: For parser computing floating point expressions use the
            following:
            {[
                let operator: string t =
                    one_of_chars "+-*/^" "One of the operators [+,-,*,/,^]"
                    |> map (String.make 1)

                in
                let primary (expr: unit -> float Located.t t): float Located.t t =
                    float
                    </>
                    parens expr
                in
                expression operator primary table
            ]}
            where [table] is the operator table describe above in the
            description of {!type:operator_table}.
        *)
    end
