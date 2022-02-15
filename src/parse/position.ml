type t =
    {line: int; column: int}

type range = t * t

let line (p: t): int =
    p.line

let column (p:t): int =
    p.column

let start: t =
    {line = 0; column = 0}

let next_column (p: t): t =
    {p with column = p.column + 1}

let next_line (p: t): t =
    {line = p.line + 1; column = 0;}

let next (c: char) (p: t): t =
    if c = '\n' then
        next_line p
    else
        next_column p


let is_less_equal (p1: t) (p2: t): bool =
    let l1, l2 = line p1, line p2
    and c1, c2 = column p1, column p2
    in
    (
        l1 < l2
        ||
        (l1 = l2 && c1 <= c2)
    )


let is_valid_range ((p1,p2): range): bool =
    is_less_equal p1 p2


let merge ((p1, _): range) ((_, p2): range): range =
    assert (is_less_equal p1 p2);
    p1, p2
