type t =
    {
        line: int;              (* Line number i.e. number of newlines since
                                   start of the file. *)

        byte_bol: int;          (* Byte position at the start of the line. *)

        byte_col: int;          (* Byte position in the current line. *)

        correction: int;        (* Byte column + correction = character column
                                 *)
    }


type range = t * t



let line (p: t): int =
    p.line


let byte_offset_bol (p: t): int =
    p.byte_bol


let byte_column p =
    p.byte_col


let byte_offset (p: t): int =
    p.byte_bol + p.byte_col


let column (p:t): int =
    p.byte_col + p.correction


let start: t = {
    line       = 0;
    byte_bol   = 0;
    byte_col   = 0;
    correction = 0;
}



let make (line: int) (column: int) (byte_bol: int) (byte_col: int): t =
    { line;
      byte_bol;
      byte_col;
      correction = byte_col - column;
    }





let advance (byte_width: int) (width: int) (p: t): t =
    {
        p with
        byte_col   = p.byte_col + byte_width;
        correction = p.correction + width - byte_width;
    }



let newline (byte_width: int) (p: t): t =
    {
        line       = p.line + 1;
        byte_col   = 0;
        byte_bol   = p.byte_bol + p.byte_col + byte_width;
        correction = 0;
    }



let next (c: char) (p: t): t =
    if c = '\n' then
        {
            line     = p.line + 1;
            byte_bol = p.byte_bol + p.byte_col + 1;
            byte_col = 0;
            correction = 0;
        }
    else
        {
            p with
            byte_col   =
                p.byte_col + 1;
            correction =
                if c = '\t' then
                    p.correction + 3
                else if c < ' ' then
                    p.correction - 1
                else
                    p.correction
        }






let correct (cor: int) (p: t): t =
    {
        p with
        correction = p.correction + cor
    }



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


let merge ((p11, p12): range) ((p21, p22): range): range =
    (
        if is_less_equal p11 p21 then
            p11
        else
            p21
    ),
    (
        if is_less_equal p12 p22 then
            p22
        else
            p12
    )
