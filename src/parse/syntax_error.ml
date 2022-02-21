module Pretty = Fmlib_pretty.Print


let header (col: int) (e: Indent.expectation): Pretty.doc =
    let open Indent in
    let open Pretty in
    match e with
    | Indent i ->
        assert (col < i);
        [
            wrap_words "indented at least";
            text (string_of_int (i - col));
            wrap_words "columns more"
        ]
        |> separated_by (group space)

    | Align i ->
        assert (col <> i);
        let delta, before_after =
            if col < i then
                i - col, "after"
            else
                col - i, "before"
        in
        [
            text "at";
            text (string_of_int delta);
            text "columns";
            text before_after
        ]
        |> separated_by (group space)

    | Align_between (i, j) ->
        let delta_i, delta_j, before_after =
            if col < i then
                i - col, j - col, "after"
            else
                col - i, col - j, "before"
        in
        [
            text "between";
            text (string_of_int delta_i);
            text "and";
            text (string_of_int delta_j);
            text "columns";
            text before_after
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
    let _ = Indent.group es in
    wrap_words
        "I have encountered something unexpected. I was expecting one of"
    <+> cut <+> cut
    <+>
    (
        Indent.group es
        |> List.map (one_group col)
        |> List.concat
        |> paragraphs
        |> nest 4
    )
    <+> cut
