module Semantic =
struct
    type t = string Located.t
end



module CP =
struct
    include Character.Make (Unit) (Float) (Semantic)
end


module Lang =
struct
    let whitespace_chars = " \t\n"
    let multiline_comment: (string * string * bool) option =
        Some ("{-", "-}", true)

    let line_comment: string option =
        Some "--"

    let is_letter (c: char): bool =
        ('A' <= c && c <= 'Z')
        ||
        ('a' <= c && c <= 'z')

    let is_digit (c: char): bool =
        ('0' <= c && c <= '9')

    let identifier_start = is_letter

    let identifier_inner (c: char): bool =
        is_letter c || is_digit c || c = '_'

    let reserved_names = []

end


module Lexeme = Lexeme.Make (CP) (Lang)

include CP
include Lexeme



let operator_table =
    [
        [ "^", Right, Binary (lift_binary ( ** ))]
        ;
        [ ("*", Left, Binary (lift_binary ( *. )));
          ("/", Left, Binary (lift_binary ( /. ))) ]
        ;
        [ ("+", Left, Both (lift_unary (~+.), lift_binary (+.)));
          ("-", Left, Both (lift_unary (~-.), lift_binary (-.))) ]
    ]


let operator: string t =
    one_of_chars "+-*/^" "[+,-,*,/,^]"
    |> map (String.make 1)


let primary (expr: unit -> float Located.t t): float Located.t t =
    float
    </>
    parens expr



let parse: Parser.t =
    make () (
        whitespace_before
            (expression operator primary operator_table)
        |> map snd (* strip location information *)
    )


open Parser

let expect_success (str: string) (pred: float -> bool) =
    let p = run_on_string str parse
    in
    if not (has_succeeded p) then
        Printf.eprintf
            "Parse has not succeeded\n  input: %s\n"
            (String.escaped str);

    has_succeeded p
    &&
    (if pred (final p) then
         true
     else
         begin
             Printf.eprintf "unexpected %g\n  input: %s\n"
                 (final p)
                 (String.escaped str);
             false
         end
    )



let%test _ =
    expect_success "-- \n  10  --   " (fun v -> v = 10.0)



let%test _ =
    expect_success
        "{- bla -} 1e3 {- more\n bla {- nested\n\n -} -}  "
        (fun v -> v = 1000.0)



let%test _ =
    expect_success "   10 ^ 4 / 10   " (fun v -> v = 1000.0)


let%test _ =
    expect_success "10 - 2-3" (fun v -> v = 5.0)


let%test _ =
    expect_success "3^2^ 2  " (fun v -> v = 81.0)


let%test _ =
    expect_success "-10.0  + 2^3 * 2.0" (fun v -> v = 6.0)
