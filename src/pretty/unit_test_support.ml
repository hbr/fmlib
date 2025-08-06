let quoted (s: string): string =
    (* Reformat a quoted string by removing a common blank prefix introduced in
       the first list (the line zero must be empty) and remove the line zero and
       the last line if it is a pure blank or empty line. The quoted string is
       assumed to look like

            {|
          line1
              line2
          line3
          line4
              line5
                 line6
          ...
            |}

       i.e. all lines have a common prefix.
    *)
    let lst = String.split_on_char '\n' s in
    let len = List.length lst in
    let rec process (i: int) (indent: int) (lst: string list)
        : string list -> string list
        = function
        | [] ->
            assert (i = len);
            lst
        | s :: rest ->
            if i = 0 then
                begin
                    assert (s = "");
                    process (i + 1) indent lst rest
                end
            else if i + 1 = len && Stdlib.String.trim s = "" then
                lst
            else
                let indent =
                    if i = 1 then
                        Fmlib_std.String.find
                            (fun c -> c <> ' ')
                            0
                            s
                    else
                        indent
                and lens = String.length s
                in
                assert (indent <= lens);
                process
                    (i + 1)
                    indent
                    (String.sub s indent (lens - indent) :: lst)
                    rest
    in
    String.concat "\n" (List.rev (process 0 0 [] lst))



let check (print: bool) (str: string) (expected: string): bool =
    let ok = str = expected in
    if not ok || print then
        Printf.printf "/---\nstr\n%s\nexpected\n%s\n\\---\n" str expected;
    ok


let test_with_ribbon
        (width: int)
        (ribbon: int)
        (print: bool)
        (doc: Print.doc)
        (expected: string)
    : bool
    =
    let res =
        let open Print
        in
        string_of (layout_with_ribbon width ribbon doc)
    in
    if print then
        Printf.printf "\n%s\n" res;
    res = expected


let test
        (width: int)
        (print: bool)
        (doc: Print.doc)
        (expected: string)
    : bool
    =
    test_with_ribbon width width print doc expected






(*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Unit Tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

let%test _ =
    let str = {|
        f
          a1
          a2
          a3
        |}
    and expect = "f\n  a1\n  a2\n  a3"
    in
    expect = quoted str
