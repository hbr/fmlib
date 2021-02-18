(** Pretty Printer: Generate nicely formatted ascii text. *)

(** {1 Overview}

    The pretty printer allows to print nicely formatted ascii text. The user
    generates a document with break hints. The primitives to generate documents
    are

    - [empty] Empty document.
    - [text str] Document which contains the string [str]. [str] should not
      contain newlines in order not to interfere with the formatter.
    - [break str] Break hint with alternative text [str].
    - [doc2 <+> doc2] Concatenation of the documents [doc1] and [doc2]
    - [nest indent doc] Indented document.
    - [group doc] Treat all top level break hints of [doc] consistently i.e.
    either print all break hints with their alternative text or as a newline.

    With these primitives a surprisingly rich set of formattings can be made.

    The user generates documents not only by using the primitives. There are a
    lot of convenience functions to make document generation easy.

    Document creation is done lazily. Only very few resources are consumed in
    producing a document. The work starts with the layout function. The layout
    function does the layout and never buffers more than one line.

    Layout is done lazily as well. The layout generates a stream of characters.
    Lines are formatted only if the characters of the line are pulled out of the
    stream.

    If you just create a document and layout it but you never use the stream,
    then no work is done.
*)

(** {2 Term Printing}

    The usage of the pretty printer is best explained by an example. Suppose
    we want to print the function application [f a b (g c d) e] where the
    function names and arguments might have different length. We create a {i
    document} which represents the structure by

    {[
    let doc =
        group (
            text "f" <+> space <+>
            indent
                2
                (stack_or_pack
                    " "
                    [text "a";
                     text "b";
                     group (
                         text "(g" <+> space <+>
                         indent
                            2
                            (stack_or_pack " " [text "c"; text "d"])
                         <+> text ")");
                     text "e"])
        )
    ]}

    where [text "blabla"] is a document with some unbreakable text, [<+>]
    concatenates two documents, [space] is a break hint whose alternative text
    is a blank, [stack_or_pack atxt [...]] stacks a list of documents separated
    by a break hint with the alternative text [atxt].

    The command

    {[let stream = layout 5 doc ]}

    creates a stream of characters which is nicely formatted using a desired
    line width of 5 characters. Since 5 characters are not enough to put any of
    the subterms completely on a line, the output is

    {[
    123456789012345
    f
      a
      b
      (g
        c
        d)
      d
    ]}

    i.e. each break hint is printed as a newline.

    If we give the pretty printer a line width of 10, it could pack the
    application [g c d] on a line and print

    {[
    123456789012345
    f
      a
      b
      (g c d)
      d
    ]}

    If the pretty printer has enough line width e.g. a line width of 15, it can
    put the whole expression on a line.

    {[
    123456789012345
    f a b (g c d) d
    ]}

    By using [stack_or_pack] we instructed the pretty printer to either print
    all break hints as newlines or all break hints with their alternative texts.
    If we use [pack] instead of [stack_or_pack], the pretty printer tries to
    pack as many arguments as possible on a line.

    E.g. with a line width of 11 and using [pack] instead of [stack_or_pack] we
    get the output

    {[
    123456789012345
    f
      a b
      (g c d) d
    ]}

    With a line width of 10 and using [pack] we get

    {[
    123456789012345
    f
      a b
      (g c d)
      d
    ]}

    because the pretty printer cannot pack [(g c d)] and [d] on a single line.

*)

(** {2 Character Stream}

    The basic type [t] of the pretty printer is a lazy character stream. I.e.
    characters are only generated if needed. The pretty printer implements the
    interface {!Std.Module_types.SOURCE} to represent a character stream. You
    can ask the stream [has_more r] whether there are more characters in the
    stream and [peek r] to get the next character. The instruction [advance r]
    returns the stream [r] advanced by one character position.

    The pretty printer has a function [string_of r] to return a string
    representation of the character stream.

    However you very rarely need a string representation of a character stream.
    All functions in {!Fmlib} are able to handle character streams.


 *)


(** {2 Formatted Paragraphs }

    There are functions to generate formatted paragraphs with indentation.

    {[
        let words =
            wrap_words "bla bla bla bla bla bla bla" <+> cut

        let doc = paragraphs [
            words;
            words;
            nest 4 words;
            words;
        ]

        let stream = layout 16 doc
    ]}

    The [stream] produces the following output
    {[  12345678901234567890

        bla bla bla bla
        bla bla bla

        bla bla bla bla
        bla bla bla

            bla bla bla
            bla bla bla
            bla

        bla bla bla bla
        bla bla bla
    ]}

*)





(** {2 Generate Documents}

    Clearly, it is tedious to write documents by hand. Usually you have some
    tree like structure and you want to generate a document from the tree
    structure.

    Let's assume you have a tree structure like

    {[
    type tree =
        { name: string; children: tree list; }

    let leaf (name: string): tree =
        {name; children = [] }

    let tree (name: string) (children: tree list): tree =
        {name; children}
    ]}

    Write a function which converts the tree structure to a document.

    {[
    let doc_of_tree (tree: tree): doc =
        let rec doc is_top tree =
            match tree.children with
            | [] ->
                text tree.name
            | _ ->
                let d =
                    parent_child
                        " " 2
                        (text tree.name)
                        (children tree.children ())
                in
                if is_top then
                    d
                else
                    char '(' <+> d <+> char ')'
        and children lst () =
            match lst with
            | [last] ->
                doc false last
            | head :: tail ->
                doc false head <+> space
                >> children tail    (* Lazy concatenation!! *)
            | [] ->
                assert false (* 'lst' is never empty *)
        in
        doc true tree
    ]}

    Then the simple command

    {[
    tree
        "f"
        [leaf "a";
         leaf "b";
         tree "g" [leaf "c"; leaf "d"];
         leaf "e"]
    |> layout 10
    ]}

    generates the character stream

    {[
    123456789012345
    f
      a
      b
      (g c d)
      e
    ]}


    Note the usage of the lazy concatentation operator [>>] in the recursive
    part of the function handling the children. This makes sure that even if the
    tree structure is hugh, the iteration over it is done only on demand. I.e.
    recursive calls are made only if the corresponding characters are needed
    when processing the character stream.
*)







(** {1 API} *)





(** {2 Character Stream} *)



type t
(** A readable character stream. *)


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




(** {2 Document} *)



(** A document. *)
type doc

(** [layout width doc] Layout the document [doc] with a the line [width]. *)
val layout: int -> doc -> t


(** [layout width ribbon doc] Layout the document [doc] with a the line [width]
    and the [ribbon] width. Note: [width] is the complete line width and
    [ribbon] is the line width minus the indentation of the current line. *)
val layout_with_ribbon: int -> int -> doc -> t


(** An empty document. *)
val empty: doc


val text: string -> doc
(** [text str] A document with the unbreakable string [str]. It is highly
    recommended that the string does not contain newlines. Newlines in a text
    string confuses the formatter. *)


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



val with_width: int -> doc -> doc
(** [with_width n doc] Format the document [doc] with line [width].*)


val with_ribbon: int -> doc -> doc
(** [with_ribbon n doc] Format the document [doc] with [ribbon] width.*)


val (<+>): doc -> doc -> doc
(** [doc1 <+> doc2] Concatentate the documents [doc1] and [doc2]. *)


val (>>): doc -> (unit -> doc) -> doc
(** [doc >> lazy_doc] Concatenate the document [doc] with the lazy document
    [lazy_doc]. *)



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
