type violation =
    | Indent of int
    | Align of int
    | Align_between of int * int


type t = {
    lb: int;          (* lower bound of the indentation set *)
    ub: int option;   (* upper bound of the indentation set *)
    abs: bool;        (* absolute alignment *)
}



let initial: t = {
    lb = 0;
    ub = None;
    abs = false
}




let check_position (pos: int) (ind: t): violation option =
    let check_indent () =
        if ind.lb <= pos then
            None
        else
            Some (Indent ind.lb)
    in
    match ind.ub with
    | None ->
        check_indent ()
    | Some ub ->
        if not ind.abs then
            check_indent ()
        else if ind.lb <= pos && pos <= ub then
            None
        else if ind.lb = ub then
            Some (Align ind.lb)
        else
            Some (Align_between (ind.lb, ub))





let token (pos: int) (ind: t): t =
    assert (check_position pos ind = None);
    if not ind.abs then
        (* Normal case. Token are all wrapped with '>=' i.e. they can appear to
           the right of the indentation set of the parent. However, if the token
           appears within the indentation set of the parent, then it restricts
           the upper bound of the indentation set of the parent. A parent can
           by definition never be indented more than all its token. *)
        match ind.ub with
        | Some ub when ub <= pos ->
            ind
        | _ ->
            {ind with ub = Some pos}
    else
        (* First token of an aligned parent. Indentation set consists of exactly
           the token position. *)
        {
            lb = pos;
            ub = Some pos;
            abs = false
        }




let align (ind:t): t =
    {
        ind with
        abs = true
    }


let left_align (ind: t): t =
    {
        ind with
        ub = Some ind.lb;
        abs = true;
    }


let start_indent (i: int) (ind: t): t =
    if not ind.abs then
        {
            lb  = ind.lb + i;
            ub  = None;
            abs = false
        }
    else
        ind


let end_indent (i: int) (ind0: t) (ind: t): t =
    if not ind0.abs then
        (* The old lower bound remains. The upper bound must be updated, if the
           new indentation has an upper bound. The upper bound is set to the
           minimum of both. *)
        match ind.ub with
        | None ->
            ind0
        | Some ub ->
            assert (ind0.lb + i <= ub);
            {
                ind0 with
                ub =
                    match ind0.ub with
                    | None ->
                        Some (ub - i)
                    | Some ub0 ->
                        Some (min ub0 (ub - i))
            }
    else
        ind
