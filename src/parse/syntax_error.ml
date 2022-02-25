module Pretty = Fmlib_pretty.Print

let plural_s (i: int) (s: string): string =
    if i = 1 then
        s
    else
        s ^ "s"


let header (col: int) (e: Indent.expectation): Pretty.doc =
    let open Indent in
    let open Pretty in
    match e with
    | Indent i ->
        assert (col < i);
        [
            wrap_words "indented at least";
            text (string_of_int (i - col));
            plural_s (i - col) "column" |> text;
            text "more"
        ]
        |> separated_by (group space)

    | Align i ->
        assert (col <> i);
        let delta, more_less =
            if col < i then
                i - col, "more"
            else
                col - i, "less"
        in
        [
            wrap_words "indented exactly";
            text (string_of_int delta);
            plural_s delta "column" |> text;
            text more_less
        ]
        |> separated_by (group space)

    | Align_between (i, j) ->
        let delta_i, delta_j, more_less =
            if col < i then
                i - col, j - col, "more"
            else
                col - i, col - j, "less"
        in
        [
            wrap_words "indented between";
            text (string_of_int delta_i);
            text "and";
            text (string_of_int delta_j);
            text "columns";
            text more_less
        ]
        |> separated_by (group space)


let one_group
        (col: int)
        ((e, lst): Indent.expectation option * string list)
    : Pretty.doc list
    =
    let open Pretty
    in
    let lst =
        List.map
            (fun str ->
                 text "- "
                 <+> (wrap_words str |> nest 2)
                 <+> cut)
            lst
    in
    match e with
    | None ->
        lst
    | Some e ->
        [
            text "- "
            <+> (header col e |> nest 2)
            <+> cut <+> cut
            <+> (paragraphs lst |> nest 4)
        ]



let document
        (col: int)
        (es: (string * Indent.expectation option) list)
    : Pretty.doc
    =
    let open Pretty
    in
    let ps =
        Indent.group es
        |> List.map (one_group col)
        |> List.concat
    in
    wrap_words
        "I have encountered something unexpected. I was expecting one of"
    <+> cut <+> cut
    <+>
    (paragraphs ps |> nest 4)
    <+> cut
