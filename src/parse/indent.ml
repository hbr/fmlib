type expectation =
    | Indent of int
    | Align of int
    | Align_between of int * int


type violation = expectation




let group
        (lst: ('a * expectation option) list)
    : (expectation option * 'a list) list
    =
    let rec grp lst =
        match lst with
        | [] ->
            []
        | (a, ea) :: lst ->
            match grp lst with
            | [] ->
                [ea, [a]]
            | (e, elst) :: lst ->
                if ea = e then begin
                    (e, a :: elst) :: lst
                end
                else begin
                    (ea, [a]) :: (e, elst) :: lst
                end
    in
    grp lst



(* Indentation set of a construct:

   It consists of a lower bound, an optional upper bound and an alignment flag.

   The upper bound is valid only if the alignment flag is set.

   The indentation of a construct is defined as the start position its first
   token.

   Tokens can appear anywhere starting from the lower bound as long as the
   alignment flag is not set. If the alignment flag is set, then a token has to
   appear between the lower bound and the optional upper bound.

   A token appearing within the allowed indentation set when the alignment flag
   is set, restricts the allowed indentation set to exactly the position of the
   start of the token and clears the alignment flag. I.e. all subsequent token
   belonging to the same parent can appear anywhere starting from the lower
   bound of the indentation set.

*)


type t = {
    lb: int;          (* lower bound of the indentation set *)
    ub: int option;   (* upper bound of the indentation set *)
    abs: bool;        (* alignment flag; if set, then we are waiting for the
                         first token which has to be in the indentation set. *)
}




let expectation (ind: t): expectation option =
    let expect () =
        if ind.lb = 0 then
            None
        else
            Some (Indent ind.lb)
    in
    match ind.ub with
    | None ->
        expect ()
    | Some ub ->
        if not ind.abs then
            expect ()
        else if ind.lb = ub then
            Some (Align ub)
        else
            Some (Align_between (ind.lb, ub))



let initial: t = {
    lb = 0;
    ub = None;
    abs = false
}




let check_position (pos: int) (ind: t): expectation option =
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
           by definition never be indented more than all its tokens. *)
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




let align (ind: t): t =
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


let end_align (ind0: t) (ind: t): t =
    if not ind.abs then
        (* flag is cleared, the aligned sequence is not empty. *)
        ind
    else
        (* the aligned sequence is empty and therefore must not have any effect
         *)
        {ind with abs = ind0.abs}



let start_indent (i: int) (ind: t): t =
    assert (0 <= i);
    if ind.abs then
        (* No effect on aligned structures which have not yet received a first
           token. *)
        ind
    else
        match ind.ub with
        | None ->
            (* It does not make sense to indent relative to something which does
               not yet have any token. *)
            ind
        | Some ub ->
            {
                lb  = ub + i;
                ub  = None;
                abs = false;
            }

let end_indent (ind0: t) (ind: t): t =
    if ind0.abs || ind0.ub = None then
        ind
    else
        ind0
