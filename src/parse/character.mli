(** Character Parser: An indentation sensitive parser which parses streams of
    characters i.e. the token type is [char].
*)

open Fmlib_std.Interfaces

module Make (State: ANY) (Final: ANY) (Semantic: ANY):
sig

    (**
    - [State]: User state.
    - [Final]: Final result type of the parser.
    - [Semantic]: Semantic error message (triggered by [fail error])
    *)



    (** {1 Final Parser} *)

    (** A parser is a consumer of tokens. At the end of consumption there is a
        result which is either a successfully parsed structure or a syntax or
        semantic error. *)
    module Parser:
    sig
        include Interfaces.PARSER
            with type token    = char
             and type final    = Final.t
             and type expect   = string * Indent.expectation option
             and type semantic = Semantic.t
             and type state    = State.t
        (** @inline *)


        val position: t -> Position.t
        (** [position p] The current position in the input stream.

            Can be called at any time.
        *)

        val line:   t -> int
        (** [line p] The current line in the input stream.

            Can be called at any time.
        *)

        val column: t -> int
        (** [column p] The current column in the input stream.

            Can be called at any time.
        *)


        val run_on_string: string -> t -> t
        (** [run_on_string str p] Run the parser [p] on the string [str]. *)


        val run_on_channel: in_channel -> t -> t
        (** [run_on_channel ic p] Run the parser [p] on input channel [ic]. *)
    end


    (** {1 Error Reporting} *)

    module Error_reporter:
    sig
        (** An error reporter is a convenience module which helps to generate
            readable error messages. *)

        open Fmlib_pretty.Print

        type t (* The type of the reporter. *)



        (** {1 Make an error reporter} *)

        val make_syntax: Parser.t -> t
        (** Make an error reported for a parser which has failed with a syntax
            error.

            Precondition: [Parser.has_failed_syntax p]
        *)

        val make:
            (Semantic.t -> Position.range)
            -> (Semantic.t -> doc)
            -> Parser.t
            -> t
        (** [make semantic_range semantic_doc p]

            Generate an error reporter from
            - [semantic_range]: Function which computes from a semantic error a
              range in the source file where the error occurred.
            - [semantic_doc]: Function which computes from a semantic error a
              pretty print document.
            - [p]: The parser which ended in an error state

            The functions for semantic errors have to be provided by the user
            because semantic errors are transparent to the parser. In case the
            parser cannot end in a semantic error (i.e. no {!fail} combinator
            has been used), then the function [fun _ -> assert false] can be
            used for both.

            Precondition: [not (Parser.has_succeeded p)]
        *)





        (** {1 Run the error reporter} *)

        (** In order to run the error reporter some kind of a stream is needed
            which represents the source code where an error has occurred. The
            reporter extracts a source snippet which contains the error and
            marks the error position. After the code snippet it adds an error
            message describing the error. *)


        val run_on_string: string -> t -> doc
        (** run the reporter on a string which represents the source
            code.*)


        val run_on_channel: in_channel -> t -> doc
        (** run the reporter on an input channel which represents the source
            code. Note that the input channel must be positioned at the start.
        *)


        val run_on_channels: in_channel -> int -> out_channel -> t -> unit
        (** [run_on_channels ic width oc r]

            Run the reporter on an input channel which represents the source
            code. Note that the input channel must be positioned at the start.

            Then write the error report with the text width [width] to the
            output channel [oc].
        *)



        (** {1 Run with inverted control} *)

        val needs_more: t -> bool
        (** Does the reporter need more characters from the source file? *)

        val put: char -> t -> t
        (** Put a character from the source file into the reporter. *)

        val put_end: t -> t
        (** Tell the reporter that there are no more characters in the input
            source. *)

        val document: t -> doc
        (** The document containing a source snippet which contains the marked
            error. *)

    end

    (** {1 Generic Combinators} *)

    include Interfaces.COMBINATOR
        with
            type state = State.t
            and type expect = string
            and type semantic = Semantic.t
    (** @inline *)



    (** {1 Location Combinator} *)

    val located: 'a t -> 'a Located.t t
    (** [located p] Parse [p] and return its result with its start and end
        position.

        Note: If [p] removes whitespace at the end, the returned end position is
        at the end of the whitespace. This is not what you usually want.
        Therefore first parse the essential part located and then remove the
        whitespace.
    *)


    val position: Position.t t
    (** The current position in the file. *)



    (** {1 Indentation Combinators} *)


    (** The indentation of a normal construct is the indentation of its leftmost
        token.  The indentation of a vertically aligned construct is the
        indentation of its first token.
    *)


    val indent: int -> 'a t -> 'a t
    (** [indent i p] Indent [p] by [i] columns relative to its parent.

        Precondition: [0 <= i]

        The indentation of [p] is defined by the indentation of its first token.
        The first token has to be indented at least [i] columns relative to the
        parent of [p]. After the first token of [p] has been parsed
        successfully, all subsequent tokens must have at least the same
        indentation.

        Note: Indentation of [p] relative to its parent only makes sense, if the
        first token of [p] is not the first token of its parent! I.e. the parent
        of [p] should have consumed at least one token before the parsing of [p]
        starts.
    *)


    (** CAUTION WITH ALIGNMENT !!

        If you want to align a certain number of constructs vertically it is {e
        mandatory} to indent the whole block of constructs. Do not indent the
        individual items to be aligned. Indent the whole block.

        Reason: The parent of the block usually has already consumed some token
        and the indentation of a construct is the position of the leftmost
        token. If you don't indent the aligned block, then it will be aligned
        with the leftmost token of the parent construct. This is usually not
        intended and a common pitfall. Any indentation e.g. zero indentation is
        ok.
    *)

    val align: 'a t -> 'a t
    (** [align p]

        Use the start position of the first token of [p] to align it with other
        constructs. If [p] does not consume any token, then [align p] has no
        effect.

        Alignment makes sense if there are at least two combinators which
        are aligned and indented. E.g. suppose there are two combinators [p] and
        [q]. Then we can form
        {[
        indent 1 (
                let* a = align p in
                let* b = align q in
                return (a,b)
        )
        ]}

        This combinator parses [p] whose first token has to be indented at least
        one column relative to its parent. And then it parses [q] whose first
        token must be aligned with the first token of [p].

        The indentation decouples the alignment of [p] and [q] with other
        aligned siblings or parents. [indent 0 ...] can be used to make the
        indentation optional.
    *)


    val left_align: 'a t -> 'a t
    (** [left_align p]

        Align a construct described by [p] at its leftmost possible column. If a
        whole block of constructs have to be vertically left aligned, then it is
        important that at least the first construct is left aligned. The
        subsequent constructs will be aligned exactly vertically. For the
        subsequent constructs [left_align] has the same effect as {!align}.
    *)


    val detach: 'a t -> 'a t
    (** [detach p] Parse [p] without any indentation and alignment restrictions.

        Detachment is needed to parse whitespace. The whitespace at the
        beginning of a line never satisfies any nontrivial indentation or
        aligment requirements.
    *)




    (** {1 Character Combinators} *)

    val expect_end: string -> 'a -> 'a t
    (** [expect_end error a] Expect the end of token stream.

        In case of success return [a].

        In case of failure return the syntax error with the error string
        [error].
    *)


    val charp: (char -> bool) -> string -> char t
    (** [charp p expect] Parse a character which satisfies the predicate [p].

        In case of failure, report the failed expectation [expect].
    *)


    val range: char -> char -> char t
    (** [range c1 c2] Parses a charager in the range between [c1] and [c2], i.e.
        a character [c] which satisfies [c1 <= c && c <= c2].*)



    val char: char -> char t
    (** [char c] Parse the character [c]. *)


    val one_of_chars: string -> expect -> char t
    (** [one_of_chars str expect]

        Parse one of the characters in the string [str]. In case of failure,
        report the failed expectation [expect].
    *)


    val string: string -> string t
    (** [string str] Parse the string [str]. *)


    val uppercase_letter: char t
    (** Parse an uppercase letter. *)


    val lowercase_letter: char t
    (** Parse a lowercase letter. *)


    val letter: char t
    (** Parse a letter. *)


    val digit_char: char t
    (** Parse a digit [0..9] and return it as character. *)


    val digit: int t
    (** Parse a digit and return it as number. *)


    val word: (char -> bool) -> (char -> bool) -> string -> string t
    (** [word first inner error]

        Parse a word which starts with a character satisfying the predicate
        [first] followed by zero or more characters satisfying the predicate
        [inner]. In case of failure add the expectation [error].
    *)


    val hex_uppercase: int t
    (** Equivalent to [range 'A' 'F'] and then converted to the corresponding
        number between [10] and [15]. *)


    val hex_lowercase: int t
    (** Equivalent to [range 'a' 'f'] and then converted to the corresponding
        number between [10] and [15]. *)



    val hex_digit: int t
    (** Parse a hexadecimal digit and return the corresponding number between
        [0] and [15]. *)


    val base64: (string -> 'r) -> (string -> 'r -> 'r) -> 'r t
    (** [base64 start next] Parse a base64 encoding into an object of type ['r].


        A base64 encoding is a sequence of zero or more base64 characters
        (A-Za-z0-9+/) grouped into sequences of 4 characters and optionally
        padded with the character [=]. Each group of 2-4 base64 characters are
        decoded into a string of 1-3 bytes.

        [start] gets the first 1-3 bytes and [next] gets all subsequent 1-3
        bytes until the end of the encoding is reached.
    *)


    val string_of_base64: string t
    (** Parse a base64 encoding and decode it into a string. *)



    (** {1 Make the Final Parser} *)

    val make: State.t -> Final.t t -> Parser.t
    (** [make state p]

        Make a parser which starts in state [state] and parses a construct
        defined by the combinator [p]. The token stream must be ended by
        [put_end], otherwise the parse won't succeed.
    *)

    val make_parser: Position.t -> State.t -> Final.t t -> Parser.t
    (** [make_parser pos state p]

        Make a parser which starts at position [pos] and state [state] and
        parses a construct defined by the combinator [p]. The parser can succeed
        even if no end token is pushed into the parser.
    *)
end
