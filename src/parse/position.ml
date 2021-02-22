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
