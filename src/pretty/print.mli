(** Pretty Printer: Generate nicely formatted ascii text. *)


(** {1 Documentation}

    {{!page-pretty} Pretty Printing Overview}
*)



(** {1 API} *)


(** {2 Types} *)



type doc
(** A document which can be pretty printed. *)


type t
(** A pretty printed document as a readable character stream. *)




(** {2 Layout a Document} *)

val layout: int -> doc -> t
(** [layout width doc] Layout the document [doc] with a the line [width] into a
    character stream. *)


val layout_with_ribbon: int -> int -> doc -> t
(** [layout width ribbon doc] Layout the document [doc] with a the line [width]
    and the [ribbon] width. Note: [width] is the complete line width and
    [ribbon] is the line width minus the indentation of the current line. *)






(** {2 Character Stream} *)


val has_more: t -> bool
(** [has_more s] Does the stream [s] have more characters to read? *)



val peek: t -> char
(** [peek s] The next character in the stream [s].

    Precondition: [has_more s]
*)


val advance: t -> t
(** [advance s] The character stream [s] advanced by one position. I.e. the
    first character popped off the stream.

    Precondition: [has_more s]
*)


val string_of: t -> string
(** [string_of s] A string representation of the stream [s]. *)


val write_to_channel: out_channel -> t -> unit
(** [write_to_channel oc s] Write the stream [s] to the output channel [oc]. *)





(** {2 Document Combinators}

    This section describes all combinators which can be used to generate
    documents and combine them into bigger documents.
*)



(** {3 Basic Combinators} *)

val empty: doc
(** An empty document. *)


val text: string -> doc
(** [text str] A document with the unbreakable string [str]. It is highly
    recommended that the string does not contain newlines. Newlines in a text
    string confuse the layouter. *)


val substring: string -> int -> int -> doc
(** [substring str start length] A document with the unbreakable string [str]
    starting at position [start] and having [length]. *)


val char: char -> doc
(** [char c] A document with the character [c]. *)


val fill: int -> char -> doc
(** [fill n c] A document with [n] repetitions of the character [c]. *)


val break: string -> doc
(** [break str] A break hint with the alternative text [str]. *)



val space: doc
(** [space] A break hint with a blank as alternative text. *)


val cut: doc
(** [cut] A break hint with an empty alternative text. *)


val (<+>): doc -> doc -> doc
(** [doc1 <+> doc2] Concatentate the documents [doc1] and [doc2]. *)


val (>>): doc -> (unit -> doc) -> doc
(** [doc >> lazy_doc] Concatenate the document [doc] with the lazy document
    [lazy_doc]. *)


val group: doc -> doc
(** [group doc]

    Treat all break hints belonging directly to [doc] consistently.
    Either print all as newlines or print all with their alternative text.

    This is the basic operation to decide break hints.

    If the whole group and all text which follows until the next break hint
    after the group fits on a line, then all break hints (directly or
    indirectly) in the group are flattened i.e. printed with their alternative
    texts.

    If the whole group does not fit, then all break hints belonging directly to
    the group are printed as effective newlines. The break hints of inner groups
    are considered separately.
*)


val nest: int -> doc -> doc
(** [nest n doc]

    The document [doc] indented by [n] blanks.

    This is the basic function to indicate a substructure to the pretty printer.
    The substructure is indented with respect to the parent document.

    The indentation is valid after each effective line break. It is usually
    convenient to group the whole substructure and put a break hint before the
    group and group the parent structure and the substructure. This makes sure
    that either the parent and the child fit on a line or the child begins on a
    newline and is indented. See the function [parent_child] below which does
    exactly that.
*)



val with_width: int -> doc -> doc
(** [with_width n doc] Format the document [doc] with line [width].

    Use this combinator if you want to format the internal document [doc] with a
    different line width than the overall document.
*)


val with_ribbon: int -> doc -> doc
(** [with_ribbon n doc] Format the document [doc] with [ribbon] width.

    Use this combinator if you want to format the internal document [doc] with a
    different ribbon width than the overall document.
*)



(** {3 Convenience Combinators} *)

val parent_child: string -> int -> doc -> doc -> doc
(** [parent_child hint indent parent child]

    Put the parent and the child in a group and separate them by a break hint
    with the alternative text [hint]. Furthermore put the child in a separate
    group.

    Equivalent to
    {[
    parent
    <+> break hint
    <+> nest indent (group child)
    |> group
    ]}
*)



val cat: doc list -> doc
(** [cat list] Concatenate all documents in the [list] of documents. *)


val separated_by: doc -> doc list -> doc
(** [separated_by sep list] Concatenate all documents in the [list] of documents
    separated by [sep]. *)


val pack: string -> doc list -> doc
(** [pack str list] Pack as much documents of the [list] of documents as possible
    into a line. I.e. separate all documents by a break hint with [str] as an
    alternative text. *)


val stack: string -> doc list -> doc
(** [stack str list] The same as [separated_by (break str) list]. *)



val stack_or_pack: string -> doc list -> doc
(** [stack_or_pack str list]
    Separate all documents of the [list] by a break hint with alternative text
    [str] and either print all break hints as newlines of with the alternative
    text [str]. *)



val wrap_words: string -> doc
(** [wrap_words str]
    Split the string [str] into words (words are substrings of [str] not
    containing blanks) and pack as many of them onto a line. *)



val paragraphs: doc list -> doc
(** [paragraphs ps]

    Print the paragraphs in [ps] separated by newlines. The function works best
    if each paragraph ends in a newline. [paragraphs ps] is equivalent to
    [separated_by cut ps].
*)
