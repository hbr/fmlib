open Fmlib_pretty

module Pretty = Fmlib_pretty.Print

type t = {
    range: Position.range;
    extra: int;
    number_width: int;
    pos:   Position.t;
    line:  string;
    doc:   Print.doc;
}


let of_range
        (extra: int)
        (range: Position.range)
    : t
    =
    assert (0 <= extra);
    assert (Position.is_valid_range range);
    let number_width =
        (Position.line (snd range) + 1)
        |> string_of_int
        |> String.length
    in
    {
        range;
        extra;
        number_width;
        pos  = Position.start;
        line = "";
        doc  = Pretty.empty;
    }


let of_position
        (extra: int)
        (pos: Position.t)
    : t
    =
    of_range extra (pos, pos)


let needs_more (ext: t): bool =
    let _, p2 = ext.range in
    Position.(line ext.pos <= line p2)


let is_in_range (p: t): bool =
    let open Position in
    let pos1, pos2 = p.range in
    line pos1 <= line p.pos + p.extra
    &&
    line p.pos <= line pos2


let is_start_line (p: t): bool =
    let open Position in
    let pos1, pos2 = p.range in
    line p.pos = line pos1
    &&
    line pos1 < line pos2


let is_end_line (p: t): bool =
    let open Position in
    let pos1, pos2 = p.range in
    line pos1 < line p.pos
    &&
    line p.pos = line pos2



let is_one_line (p: t): bool =
    let open Position in
    let pos1, pos2 = p.range in
    line p.pos = line pos1
    &&
    line pos1 = line pos2


let source_separator: string =
    " | "


let source_indent (p: t): int =
    p.number_width
    +
    String.length source_separator



let source_line (p: t): Pretty.doc =
    let str =
        Position.line p.pos + 1 |> string_of_int
    in
    let n = p.number_width - String.length str
    in
    assert (0 <= n);
    Pretty.(
        fill n ' '
        <+>
        text str
        <+>
        text source_separator
        <+>
        text p.line
        <+>
        cut
    )


let start_line_marker (p: t): Pretty.doc =
    let col = Position.column (fst p.range) in
    Pretty.(
        fill (source_indent p + col) ' '
        <+>
        char 'v'
        <+>
        fill 10 '-'
        <+>
        cut
    )


let end_line_marker (p: t): Pretty.doc =
    let col = Position.column (snd p.range)
    and ind = source_indent p
    in
    Pretty.(
        fill ind ' '
        <+>
        fill (col - ind) '-'
        <+>
        char '^'
        <+>
        cut
    )


let one_line_marker (is_last: bool) (p: t): Pretty.doc =
    let open Position in
    let pos1, pos2 = p.range in
    let c1 = Position.column pos1
    and c2 = Position.column pos2
    in
    assert (line pos1 = line pos2);
    assert (c1 <= c2);
    let len = c2 - c1 in
    let len = max len 1 in
    let open Pretty
    in
    let annotation =
        if len = 1 && c1 < String.length p.line
        then
            let ch = p.line.[c1] in
            if ch < ' ' || Char.chr 127 <= ch then
                text (" nonprintable '" ^ Char.escaped ch ^ "'")
            else
                empty
        else if len = 1 && c1 = String.length p.line then
            if is_last then
                text " end of input"
            else
                text " end of line"
        else
            Pretty.empty
    in
    Pretty.(
        fill (source_indent p + c1) ' '
        <+>
        fill len '^'
        <+>
        annotation
        <+>
        cut
    )


let receive_char (is_last: bool) (c: char) (p: t): t =
    let pos = Position.next c p.pos in
    if c <> '\n' then
        {
            p with
            pos;
            line = p.line ^ String.make 1 c;
        }
    else if is_in_range p then
        let open Pretty in
        let doc =
            if is_start_line p then
                start_line_marker p <+> source_line p
            else if is_one_line p then
                source_line p <+> one_line_marker is_last p
            else if is_end_line p then
                source_line p <+> end_line_marker p
            else
                source_line p
        in
        {
            p with
            pos;
            line = "";
            doc  = p.doc <+> doc;
        }
    else
        {
            p with
            pos;
            line = "";
        }


let put: char -> t -> t =
    receive_char false


let put_end: t -> t =
    receive_char true '\n'


let document (p: t): Pretty.doc =
    p.doc


let run_on_string  = Run_on.string  needs_more put put_end

let run_on_channel = Run_on.channel needs_more put put_end
