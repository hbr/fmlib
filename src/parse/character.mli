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

    module Parser:
    sig
        include Interfaces.PARSER
            with type token    = char
             and type final    = Final.t
             and type expect   = string * Indent.violation option
             and type semantic = Semantic.t
             and type state    = State.t


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
    end



    (** {1 Generic Combinators} *)

    include Interfaces.COMBINATOR
        with
            type state = State.t
            and type expect = string
            and type semantic = Semantic.t



    (** {1 Location Combinator} *)

    val located: 'a t -> 'a Located.t t
    (** [located p] Parse [p] and return its result with its start and end
        position.

        Note: If [p] parses strips whitespace at the end, the returned end
        position is at the end of the whitespace. This is not what you usually
        want. Therefore first parse the essential part located and then strip
        the whitespace.
    *)



    (** {1 Indentation Combinators} *)


    val indent: int -> 'a t -> 'a t
    (** [indent i p] Indent [p] by [i] columns relative to its parent.

        Precondition: [0 <= i]
    *)


    val align: 'a t -> 'a t
    (** [align p]

        Set the indentation set of [p] to [{col}] where [col] is the column
        position of its first character. Fail, if [col] is not in the
        indentation set.
    *)


    val left_align: 'a t -> 'a t
    (** [left_align p]

        Set the indentation set of [p] to [{col}] where [col] is the column
        position of its first character. Fail, if [col] is not the lower bound
        of the indentation set. I.e. [p] is left aligned in its indentation set.
    *)


    val detach: 'a t -> 'a t
    (** [detach p] Parse [p] without any indentation and alignment restrictions.
    *)


    val zero_or_more_aligned: 'r -> ('a -> 'r -> 'r) -> 'a t -> 'r t
    (** [zero_or_more_aligned start next p]

        Parse an indented block of zero or more aligned constructs [p].

        Equivalent to
        {[
            zero_or_more start next (align p) |> align |> indent 1
        ]}
    *)



    val one_or_more_aligned: ('a -> 'r) -> ('a -> 'r -> 'r) -> 'a t -> 'r t
    (** [zero_or_more_aligned first next p]

        Parse an indented block of one or more aligned constructs [p].

        Equivalent to
        {[
            one_or_more first next (align p) |> align |> indent 1
        ]}
    *)





    (** {1 Character Combinators} *)


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


    val digit: int t
    (** Parse a digit. *)


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
