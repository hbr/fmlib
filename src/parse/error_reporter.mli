(** Convenience module to generate
    readable error messages.

    An error reporter is a parser which reparses the input stream, extracts the
    failed code snippet and prints the error nicely formatted.

    For semantic errors, the module needs support from the user to convert a
    semantic error into a pretty print document.
*)



(** Needed functions from a failed parser i.e. a parser which has failed either
    with a syntax error or a semantic error.
*)
module type FAILED_PARSER =
sig
    type t
    type expect = string * Indent.expectation option
    type semantic
    val has_failed_syntax: t -> bool
    val failed_expectations: t -> expect list
    val failed_semantic: t -> semantic
    val position: t -> Position.t
end



(** Generate the error reporter from a failed parser.
*)
module Make (Parser: FAILED_PARSER):
sig
    (** The [Parser] has to be a failed parser i.e. a parser which has ended
        either with a syntax error or a semantic error.

    *)

    open Fmlib_pretty.Print

    (** {1 Types} *)


    type semantic = Parser.semantic (** Type of the semantic error. *)


    type t (** The type of the reporter. *)



    (** {1 Make an error reporter} *)

    val make_syntax: Parser.t -> t
    (** Make an error reported for a parser which has failed with a syntax
        error.

        Precondition: [Parser.has_failed_syntax p]
    *)

    val make:
        (semantic -> Position.range)
        -> (semantic -> doc)
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
        parser cannot end in a semantic error (i.e. no [fail] combinator
        has been used), then use {!make_syntax}.
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

    type item = char
    (** This type makes the error reporter an instance of
        {!Fmlib_std.Interfaces.SINK}. *)

    val needs_more: t -> bool
    (** Does the reporter need more characters from the source file? *)

    val put: char -> t -> t
    (** Put a character from the source file into the reporter. *)

    val put_end: t -> t
    (** Tell the reporter that there are no more characters in the input
        source. *)

    val document: t -> doc
    (** The document containing a source snippet which contains the marked
        error and the error message. *)
end
