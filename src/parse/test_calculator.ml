module CP =
    Character.Make
        (Unit)                  (* No state needed. *)
        (Int)                   (* The parser returns a number. *)
        (String)                (* The possible semantic error. *)
open CP

let whitespace: int t =
    skip_zero_or_more (char ' ')

type addop = Plus  | Minus
type mulop = Times | Divide


let operator (c: char) (op: 'a): 'a t =
    map (fun _ -> op) (char c)


let addop: addop t =
    (* Parse an addition operator. *)
    let* op =
        operator '+' Plus </> operator '-' Minus
    in
    let* _ = whitespace in      (* strip whitespace *)
    return op


let mulop: mulop t =
    (* Parse a multiplication operator. *)
    let* op =
        operator '*' Times </> operator '/' Divide
    in
    let* _ = whitespace in      (* strip whitespace *)
    return op


let number: int t =
    (* Parse one number. *)
    let* v =
        one_or_more_fold_left
            (fun d -> return d)
            (fun v d -> 10 * v + d |> return)
            digit
    in
    let* _ = whitespace in      (* strip whitespace *)
    return v


let parenthesized (p: unit -> 'a t): 'a t =
    let* _ = char '(' in
    let* _ = whitespace in
    let* x = p () in
    let* _ = char ')' in
    let* _ = whitespace in
    return x


let rec expr (): int t =
    (* Parse a sum [a + b - c ...]. *)
    one_or_more_separated
        return
        (fun s op x ->
             match op with
             | Plus ->
                 s + x |> return
             | Minus ->
                 s - x |> return)
        (product ())
        addop

and atomic (): int t =
    number
    </>
    parenthesized expr


and factors (opnd1: int): int t =
    (* Parse the factors of a product. *)
    (
        let* op    = mulop in
        let* opnd2 = atomic ()
        in
        match op with
        | Times ->
            factors (opnd1 * opnd2)
        | Divide ->
            if opnd2 = 0 then
                fail "division by zero"
            else
                factors (opnd1 / opnd2)
    )
    </>
    return opnd1


and product (): int t =
    (* Parse a product [f1 * f2 / f3 ...]. *)
    let* n = atomic () in
    factors n



let calculator: Parser.t =
    make () (expr ())


let%test _ =
    let p = Parser.run_on_string "(1 + 2) * 6 / 2 -1" calculator in
    Parser.has_succeeded p
    &&
    Parser.final p = 8


let%test _ =
    let p = Parser.run_on_string "1 / 0" calculator in
    Parser.has_failed_semantic p
