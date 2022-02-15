open Fmlib_std

type assoc =
    | Left
    | Right


type info = {
    precedence: int;
    associativity: assoc;
}

let left (p: int): info =
    assert (0 <= p);
    {
        precedence = p;
        associativity = Left;
    }

let right (p: int): info =
    assert (0 <= p);
    {
        precedence = p;
        associativity = Right;
    }

module Map =
struct
    include Btree.Map (Char)
    let find c map =
        match find_opt c map with
        | None ->
            assert false
        | Some r ->
            r
end



let operators: string =
    "+-*/^!&|"


let map: 'info Map.t =
    let open Map in
    empty
    |> add '|' (left 10)
    |> add '&' (left 20)
    |> add '!' (left 30)
    |> add '+' (left 40)
    |> add '-' (left 40)
    |> add '*' (left 50)
    |> add '/' (left 50)
    |> add '^' (right 60)


let is_left0 (o1: char) (o2: char): bool =
    let i1 = Map.find o1 map
    and i2 = Map.find o2 map
    in
    i1.precedence > i2.precedence
    ||
    (
        i1.precedence = i2.precedence
        &&
        i1.associativity = Left
    )



module CP =
    Character.Make (Unit) (String) (String)

open CP


let is_left (a: char) (b: char): bool t =
    return (is_left0 a b)

let operator: char t =
    one_of_chars operators ("one of \"" ^ operators ^ "\"")

let make_unary (u: char) (a: string): string t =
    return ("(" ^ String.one u ^ a ^ ")")

let make_binary (a: string) (o: char) (b: string): string t =
    return ("(" ^ a ^ String.one o ^ b ^ ")")


let primary: string t =
    map String.one letter

let lpar: char t =
    (
        let* _ = char '(' in
        return ')'
    )
    </>
    (
        let* _ = char '[' in
        return ']'
    )
    </>
    (
        let* _ = char '{' in
        return '}'
    )

let rpar (p: char): char t =
    charp (fun c -> c = p) ("'" ^ String.one p ^ "'")


let rec exp (): string t =
    let prim () =
        parenthesized
            (fun _ a _ -> return a)
            lpar
            exp
            rpar
        </>
        primary
    in
    operator_expression
        (prim ())
        (Some operator)
        operator
        is_left
        make_unary
        make_binary


let parse: Parser.t =
    make
        ()
        (exp ())






let%test _ =
    let p = Parser.run_on_string "a" parse in
    Parser.(
        has_succeeded p
        &&
        column p = 1
        &&
        final p = "a"
    )




let%test _ =
    let p = Parser.run_on_string "a+-b+c" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "((a+(-b))+c)"
    )




let%test _ =
    let p = Parser.run_on_string "a+b*c" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "(a+(b*c))"
    )




let%test _ =
    let p = Parser.run_on_string "[a*{b+[[[(((c)))]]]}]^d" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "((a*(b+c))^d)"
    )




let%test _ =
    let p = Parser.run_on_string "a*-b*c" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "(a*(-(b*c)))"
    )




let%test _ =
    let p = Parser.run_on_string "a+b^c^d" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "(a+(b^(c^d)))"
    )




let%test _ =
    let p = Parser.run_on_string "a+b*c^d" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "(a+(b*(c^d)))"
    )




let%test _ =
    let p = Parser.run_on_string "a+!b+c" parse in
    Parser.(
        has_succeeded p
        &&
        final p = "(a+(!(b+c)))"
    )
