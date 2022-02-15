open Fmlib_std

type assoc =
    | Left
    | Right


type info = int * assoc


module Map:
sig
    val find: char -> info
end
=
struct
    include Map.Make (Char)

    let add_left (c: char) (i: int) (m: 'info t): 'info t =
        add c (i, Left) m

    let add_right (c: char) (i: int) (m: 'info t): 'info t =
        add c (i, Right) m

    let map: 'info t =
        empty
        |> add_left  '+' 0
        |> add_left  '-' 0
        |> add_left  '*' 1
        |> add_left  '/' 1
        |> add_right '^' 2

    let find (c: char): info =
        match find_opt c map with
        | None ->
            assert false (* cannot happen *)
        | Some i ->
            i
end


module Semantic =
struct
    type t = Position.range * string
end


module CP = Character.Make (Unit) (Int) (Semantic)

open CP


type operator = Position.range * char
type operand  = Position.range * int


let whitespace: int t =
    char ' ' </> char '\n'
    |> skip_zero_or_more
    |> no_expectations


let lexeme (p: 'a t): 'a t =
    let* a = p in
    let* _ = whitespace in
    return a


let unary_operator: operator t =
    lexeme (char '-' |> located)


let binary_operator: operator t =
    let op_chars = "+-*/^"
    in
    one_of_chars op_chars "binary operator"
    |>
    located
    |>
    lexeme


let number: operand t =
    one_or_more_fold_left
        (fun d -> return d)
        (fun v d -> 10 * v + d |> return)
        digit
    |>
    located
    |>
    no_expectations
    <?>
    "number"
    |>
    lexeme


let lpar: char t =
    lexeme (
        map (fun _ -> ')') (char '(')
        </>
        map (fun _ -> ']') (char '[')
    )
    <?>
    "opening parenthesis '(' or '['"


let rpar (c: char): char t =
    lexeme (char c)


let is_left ((_,c1): operator) ((_,c2): operator): bool t =
    let (p1, a1) = Map.find c1
    and (p2, _ ) = Map.find c2
    in
    return (
        p1 > p2
        ||
        (
            p1 = p2
            &&
            a1 = Left
        )
    )


let make_unary
        (((p1,_), u): operator)
        (((_,p2), a): operand)
    : operand t
    =
    assert (u = '-');
    return ((p1, p2), (-1) * a)


let power (a: int) (b: int): int =
    assert (b <> 0);
    let rec pow b res =
        if b = 0 then
            res
        else
            pow (b - 1) (a * res)
    in
    pow b 1


let make_binary
        (((p1,_), a): operand)
        ((_, o): operator)
        (((pb,p2), b): operand)
    : operand t
    =
    match o with
    | '+' ->
        return ((p1,p2), a + b)
    | '-' ->
        return ((p1,p2), a - b)
    | '*' ->
        return ((p1,p2), a * b)
    | '/' ->
        if b = 0 then
            fail ((pb, p2), "Division by zero")
        else
            return ((p1,p2), a / b)
    | '^' ->
        if b < 0 then
            fail ((pb, p2), "Negative exponent")
        else
            return ((p1,p2), power a b)
    | _ ->
        assert false (* cannot happen *)



let rec expr (): operand t =
    let primary (): operand t =
        parenthesized
            (fun _ a _ -> return a)
            lpar
            expr
            rpar
        </>
        number
    in
    operator_expression
        (primary ())
        (Some unary_operator)
        binary_operator
        is_left
        make_unary
        make_binary

let parse: Parser.t =
    make () (let* _ = whitespace in expr () |> map snd)









(* Unit Tests
 * ==========
*)




let%test _ =
    let open Parser in
    let p = run_on_string " 1 +,2 + 2 " parse
    in
    has_failed_syntax p
    &&
    column p = 4




let%test _ =
    let open Parser in
    let p = run_on_string "[1 + 2 ) + 2 " parse
    in
    has_failed_syntax p
    &&
    column p = 7




let%test _ =
    let p = Parser.run_on_string " 105 + 10 ^ 3" parse in
    Parser.(
        has_succeeded p
        &&
        final p = 1105
    )




let%test _ =
    let p = Parser.run_on_string "1 + - 2 * 3" parse in
    Parser.(
        has_succeeded p
        &&
        final p = -5
    )




let%test _ =
    let p = Parser.run_on_string "10 - 2 - 3" parse in
    Parser.(
        has_succeeded p
        &&
        final p = 5
    )




let%test _ =
    (*                            01234567890123456789 *)
    let p = Parser.run_on_string "1 + 2 ^  - 3" parse in
    Parser.(
        has_failed_semantic p
        &&
        let (p1, p2), err = failed_semantic p
        in
        err = "Negative exponent"
        &&
        Position.column p1 =  9
        &&
        Position.column p2 = 12
    )





let%test _ =
    (*                            01234567890123456789 *)
    let p = Parser.run_on_string "1 + 2 ^  [3 / 0] " parse in
    Parser.(
        has_failed_semantic p
        &&
        let (p1, p2), err = failed_semantic p
        in
        err = "Division by zero"
        &&
        Position.column p1 = 14
        &&
        Position.column p2 = 15
    )
