(** Pretty Printer: Generate nicely formatted ascii text. *)



(** {1 Documentation}

    {{!page-pretty} Pretty Printing Overview}
*)



(** {1 API} *)


(** Lazy stream of characters *)
module Stream:
    Fmlib_std.Interfaces.SOURCE
    with type item := char


type t
(** A document which can be pretty printed. *)





(** {2 Create Documents} *)



val empty: t
(** An empty document. *)

val text: string -> t
(** [text str] A document with the unbreakable string [str]. It is highly
    recommended that the string does not contain newlines. Newlines in a text
    string confuse the layouter. *)



val substring: string -> int -> int -> t
(** [substring str start length] A document with the unbreakable string [str]
    starting at position [start] and having [length].

    This is a useful function if you want to create documents from a larger
    string without splittig the string. {!val:substring} is a reference into a
    possible larger string.
*)


val fill: int -> char -> t
(** [fill n c] A document with [n] repetitions of the character [c]. *)


val char: char -> t
(** [char c] A document with the character [c]. *)


val break: string -> t
(** [break str] A break hint with the alternative text [str]. *)

val cut: t
(** [cut] A break hint with an empty alternative text. *)


val space: t
(** [space] A break hint with one blank as alternative text. *)


val (<+>): t -> t -> t
(** [x <+> y] Concatentate the documents [x] and [y]. *)

val (+|):  t -> t Lazy.t -> t
(** [doc1 <+> doc2] Concatentate the document [x] with the lazy document
    [y].

    Lazy documents are useful for large documents.
*)

val (|+|): t Lazy.t -> t Lazy.t -> t
(** [doc1 <+> doc2] Concatentate the lazy document [x] with the lazy document
    [y].

    Lazy documents are useful for large documents.
*)



val cat: t list -> t
(** [cat list] Concatenate all documents in the [list] of documents. *)


val separated_by: t -> t list -> t
(** [separated_by sep list] Concatenate all documents in the [list] of documents
    separated by [sep]. *)


val paragraphs: t list -> t
(** [paragraphs ps]

    Print the paragraphs in [ps] separated by newlines. The function works best
    if each paragraph ends in a newline. [paragraphs ps] is equivalent to
    [separated_by cut ps].
*)


val nest: int -> t -> t
(** [nest n x]

    The document [x] indented by [n] blanks.

    This is the basic function to indicate a substructure to the pretty printer.
    The substructure is indented with respect to the parent document.

    The indentation is valid after each effective line break. It is usually
    convenient to group the whole substructure and put a break hint before the
    group and group the parent structure and the substructure. This makes sure
    that either the parent and the child fit on a line or the child begins on a
    newline and is indented. See the function {!val:parent_child} below which
    does exactly that.
*)


val nest_lazy: int -> t Lazy.t -> t
(** The same as {!val:nest} where the indented document is a lazy document. *)


val group: t -> t
(** [group x]

    Treat all break hints belonging directly to [x] consistently.
    Either print all as newlines or print all with their alternative text.

    This is the basic operation to decide break hints.

    If the whole group and all text which follows until the next break hint
    after the group fits on a line, then all break hints (directly or
    indirectly) in the group are flattened i.e. printed with their alternative
    texts.

    If the whole group does not fit, then all break hints belonging directly to
    the group are printed as effective newlines. The breaks of inner groups
    might be flattened depending on the length of the inner groups.

    Grouping is idempotent, i.e. [group (group x)] is the same as [group x].
*)


val parent_child: string -> int -> t -> t -> t
(** [parent_child hint indent parent child]

    Put the parent and the child in a group and separate them by a break hint
    with the alternative text [hint]. Furthermore put the child in a separate
    group.

    Equivalent to
    {[
    parent
    <+> nest indent (break hint <+> group child)
    |> group
    ]}
*)


val pack: string -> t list -> t
(** [pack str list] Pack as many documents of the [list] of documents as possible
    into a line. I.e. separate all documents by a break hint with [str] in an
    own group as an alternative text. *)


val stack: string -> t list -> t
(** [stack str list] Put a break hint between all documents in the list.

    No grouping is done. I.e. if the list does not belong to any group or is in
    a group where all breaks are effective, then each document in the list is
    printed on a separated line.

    The same as [separated_by (break str) list].
*)


val stack_or_pack: string -> t list -> t
(** [stack_or_pack str list]
    Separate all documents of the [list] by a break hint with alternative text
    [str] and either print all break hints as newlines of with the alternative
    text [str].

    This is effectively [stack str list |> group].

    It should not be confused with [stack str list] where each break hint can be
    flattened individually.
*)

val wrap_words: string -> t
(** [wrap_words str]

    Split the string [str] into words (words are substrings of [str] not
    containing whitespace, newline, CR and tab are treated as whitespace) and
    pack as many of them onto a line. *)


val wrap_words_list: string list -> t
(** The same as {!val:wrap_words} [str] where [str] is the string of all
    concatenated strings in the list separated by a whitespace character.
*)


(** {2 Layout Documents} *)

val layout: int -> t -> Stream.t
(** [layout width doc] Layout the document [doc] with a the line [width] into a
    character stream. *)


val layout_with_ribbon: int -> int -> t -> Stream.t
(** [layout width ribbon x] Layout the document [x] with a the line [width]
    and the [ribbon] width. Note: [width] is the desired complete line width and
    [ribbon] is the desired width of a line without its indentation. *)



(** {2 Write or Convert to String} *)

val write_to_channel: out_channel -> Stream.t -> unit
(** Write a character stream to an output channel. *)

val to_string: Stream.t -> string
(** Convert a character stream to a string. *)
