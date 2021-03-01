open Fmlib_std

module CP = Character.Make (Unit) (Char)   (Unit)
module IP = Character.Make (Unit) (Int)    (Unit)
module SP = Character.Make (Unit) (String) (Unit)



(*
One Token
------------------------------------------------------------
*)


let%test _ =
    let open CP in
    let p = Parser.run_on_string "a" (make () letter) in
    Parser.has_succeeded p
    &&
    Parser.column p = 1
    &&
    Parser.final p = 'a'
    &&
    Parser.lookaheads p = ([||], true)


let%test _ =
    let open CP in
    let p = Parser.run_on_string "," (make () letter) in
    Parser.has_failed_syntax p
    &&
    Parser.column p = 0
    &&
    Parser.lookaheads p = ([|','|], false)


let%test _ =
    let open CP in
    let p = Parser.run_on_string "ab" (make () letter) in
    Parser.has_failed_syntax p
    &&
    Parser.column p = 1
    &&
    Parser.lookaheads p = ([|'b'|], false)


let%test _ =
    let open CP in
    let p =
        Parser.run_on_string
            "a" (make () (char 'a' </> char 'b')) in
    Parser.has_succeeded p
    &&
    Parser.final p = 'a'


let%test _ =
    let open CP in
    let p =
        Parser.run_on_string
            "b" (make () (char 'a' </> char 'b')) in
    Parser.has_succeeded p
    &&
    Parser.final p = 'b'


let%test _ =
    let open IP in
    let p =
        Parser.run_on_string
            "F" (make () hex_digit) in
    Parser.(
        has_succeeded p
        &&
        final p = 15)




(*
Backtracking
------------------------------------------------------------
*)

let%test _ =
    let open SP in
    let p =
    Parser.run_on_string
        "(a)" (make () (string "(a)" </> string "(b)"))
    in
    Parser.has_succeeded p
    &&
    Parser.final p = "(a)"


let%test _ =
    let open SP in
    let p =
    Parser.run_on_string
        "(b)" (make () (string "(a)" </> string "(b)"))
    in
    Parser.has_failed_syntax p
    &&
    Parser.column p = 1
    &&
    Parser.failed_expectations p = ["'a'", None]


let%test _ =
    let open SP in
    let p =
    Parser.run_on_string
        "(b)"
        (make
             ()
             (backtrack (string "(a)") "(a)" </> string "(b)"))
    in
    Parser.has_succeeded p
    &&
    Parser.final p = "(b)"






(*
Nested Backtracking
------------------------------------------------------------
*)

let abcdef = SP.(
     backtrack
         (let* s1 = string "abc" in
          let* s2 =
              backtrack (string "def") "def"
              </>
              string "dez"
          in
          return (s1 ^ s2))
         "abcdef"
 )

let%test _ =
    let open SP in
    let p =
    Parser.run_on_string
        "abcdeg"
        (make () abcdef)
    in
    Parser.has_failed_syntax p
    &&
    Parser.column p = 0
    &&
    Parser.failed_expectations p = ["abcdef", None]


let%test _ =
    let open SP in
    let p =
        Parser.run_on_string
            "abcdef"
            (make () abcdef)
    in
    Parser.has_succeeded p
    &&
    Parser.final p = "abcdef"


let%test _ =
    let open SP in
    let p =
        Parser.run_on_string
            "abcdez"
            (make () abcdef)
    in
    Parser.has_succeeded p
    &&
    Parser.final p = "abcdez"




(*
Followed by and not followed by
------------------------------------------------------------
*)

let%test _ =
    (* "abc" followed by "def". Success case. *)
    let open SP in
    let p =
        let* str = string "abc" in
        let* _   = followed_by (string "def") "def" in
        return str
    in
    let p =
        Parser.run_on_string
            "abcdef"
            (make_parser Position.start () p)
    in
    Parser.has_succeeded p
    &&
    Parser.final p = "abc"
    &&
    Parser.column p = 3


let%test _ =
    (* "abc" followed by "def". Failure case. *)
    let open SP in
    let p =
        let* str = string "abc" in
        let* _   = followed_by (string "def") "def" in
        return str
    in
    let p =
        Parser.run_on_string
            "abcdez"
            (make_parser Position.start () p)
    in
    Parser.has_failed_syntax p
    &&
    Parser.column p = 3
    &&
    Parser.failed_expectations p = ["def", None]



let%test _ =
    (* "abc" not followed by "def". Success case. *)
    let open SP in
    let p =
        let* str = string "abc" in
        let* _   = not_followed_by (string "def") "" in
        return str
    in
    let p =
        Parser.run_on_string
            "abcdez"
            (make_parser Position.start () p)
    in
    Parser.has_succeeded p
    &&
    Parser.final p = "abc"
    &&
    Parser.column p = 3



let%test _ =
    (* "abc" not followed by "def". Failure case. *)
    let open SP in
    let p =
        let* str = string "abc" in
        let* _   = not_followed_by (string "def") "def" in
        return str
    in
    let p =
        Parser.run_on_string
            "abcdef"
            (make_parser Position.start () p)
    in
    Parser.has_failed_syntax p
    &&
    Parser.column p = 3
    &&
    Parser.failed_expectations p = ["def", None]




(*
Indentation Sensitivity
------------------------------------------------------------
*)

module Indent_sensitive (Final: Fmlib_std.Interfaces.ANY) =
struct
    module Basic = Character.Make (Unit) (Final) (Unit)

    include Basic

    let whitespace: int t =
        char ' ' </> char '\n' <?> "whitespace"
        |> (fun p ->
            skip_zero_or_more p >>= clear_last_expectation)
        |> detach


    let skip_trailing_ws (p: 'a t): 'a t =
        let* a = p in
        let* _ = whitespace in
        return a

    let char_ws (c: char): char t =
        skip_trailing_ws (char c)


    let string_of_expectations (p: Parser.t): string =
        assert (Parser.has_failed_syntax p);
        "["
        ^
        String.concat
            ","
            (List.map
                 (fun (msg,vio) ->
                      let open Indent in
                      "(" ^ msg ^ ", " ^
                      (match vio with
                       | None ->
                          "None"
                       | Some (Indent i) ->
                           "Indent " ^ string_of_int i
                       | Some (Align i) ->
                           "Align " ^ string_of_int i
                       | Some (Align_between (i, j)) ->
                           "Align_between "
                           ^ string_of_int i ^ "," ^ string_of_int j)
                      ^
                      ")")
            (Parser.failed_expectations p))
        ^
        "]"
end



let%test _ =
    (* A character left aligned. *)
    let open Indent_sensitive (Char) in
    let p =
        (let* _ = whitespace in
         char 'a' |> left_align)
        |>
        make ()
        |>
        Parser.run_on_string "   \na"
    in
    Parser.has_succeeded p
    &&
    Parser.final p = 'a'
    &&
    Parser.line p = 1
    &&
    Parser.column p = 1



let%test _ =
    (* A character left aligned, but not found. *)
    let open Indent_sensitive (Char) in
    let p =
        (let* _ = whitespace in
         char 'a' |> left_align)
        |>
        make ()
        |>
        Parser.run_on_string "   \n\n\n\n a"
    in
    Parser.has_failed_syntax p
    &&
    Parser.line p = 4
    &&
    Parser.column p = 1
    &&
    Parser.failed_expectations p =
        ["'a'", Some (Indent.Align 0)]


let%test _ =
    (* Two characters indented and aligned *)
    let open Indent_sensitive (Char) in
    let p =
        (let* _  = whitespace in
         let* c0 = char_ws 'a' |> align in
         let* _  =
             (let* _ = char_ws 'b' |> align in
              char_ws 'c' |> align)
             |> align |> indent 1
         in
         return c0)
        |> make ()
        |> Parser.run_on_string
               "\n\
                \ a\n\
                \    b\n\
                \    c"
    in
    Parser.has_succeeded p
    &&
    Parser.line p = 3
    &&
    Parser.column p = 5



let%test _ =
    (* Two characters indented and wrongly aligned *)
    let open Indent_sensitive (Char) in
    let p =
        (let* _  = whitespace in
         let* c0 = char_ws 'a' |> align in
         let* _  =
             (let* _ = char_ws 'b' |> align in
              char_ws 'c' |> align)
             |> align |> indent 1
         in
         return c0)
        |> make ()
        |> Parser.run_on_string
               "\n\
                \ a\n\
                \    b\n\
                \     c"
    in
    Parser.has_failed_syntax p
    &&
    Parser.line p = 3
    &&
    Parser.column p = 5
    &&
    Parser.failed_expectations p = ["'c'", Some (Align 4)]
