(* This is an example of a parser recognizing a simplified json grammar by
 * separating the lexer and the parser.
 *)


module Token =
struct
    type tp =
        | End
        | String
        | Number
        | Bool
        | Colon
        | Comma
        | Lbrace
        | Rbrace
        | Lbrack
        | Rbrack

    type t = tp * string
end


module Token_plus =
struct
    type t = Position.range * Token.t
end





module Lexer =
struct
    module CP = Character.Make (Unit) (Token_plus) (Fmlib_std.Void)

    open CP




    (* Whitespace
     * ==========
     *   - blanks
     *   - newlines
     *   - comments of the form "// xxxxx" until the end of line or end of
     *     input
     *   - multline comments of the form
     *        /* xxxx
     *           xxxxxx */
     *)



    let blank_or_newline: unit t =
        let* _ = char ' ' </> char '\n' </> char '\r' in
        return ()


    let line_comment: unit t =
        let* _ =
            backtrack (string "//") {|"//"|}
        in
        let* _ =
            skip_zero_or_more
                (charp
                     (fun c -> c <> '\n')
                     "any char except newline")
        in
        return ()


    let multi_line_comment: unit t =
        let rec rest star =
            (* parse the remaining part of a multiline comment after the initial
             * "/*". The flag [star] indicates that the previous character of
             * the rest has been a '*'.
             *)
            let* c =
                charp (fun _ -> true) "any character in a comment"
            in
            if not star && c = '*' then
                rest  true
            else if star && c = '/' then
                return ()
            else
                rest  false
        in
        let* _ =
            backtrack (string "/*") {|"/*"|}
        in
        rest false


    let whitespace: int t =
        skip_zero_or_more
            (blank_or_newline </> line_comment </> multi_line_comment)
        |> no_expectations








    (* Specific tokens
     * ===============
     *)

    let colon: Token.t t =
        let* _ = char ':' in
        return (Token.Colon, ":")

    let comma: Token.t t =
        let* _ = char ',' in
        return (Token.Comma, ",")

    let lbrace: Token.t t =
        let* _ = char '{' in
        return (Token.Lbrace, "{")

    let rbrace: Token.t t =
        let* _ = char '}' in
        return (Token.Rbrace, "}")

    let lbrack: Token.t t =
        let* _ = char '[' in
        return (Token.Lbrack, "[")

    let rbrack: Token.t t =
        let* _ = char ']' in
        return (Token.Rbrack, "]")

    let string: Token.t t =
        let* _    = char '"' <?> "string" in
        let* lst  =
            zero_or_more
                (map
                     (fun c -> String.make 1 c)
                     (charp
                          (fun c -> ' ' <= c && c <= '~' && c <> '"')
                          "printable character")
                )
        in
        let* _ = char '"' in
        return (Token.String, String.concat "" lst)


    let number: Token.t t =
        let is_digit c = '0' <= c && c <= '9'
        in
        map
            (fun str -> Token.Number, str)
            (word is_digit is_digit "number")


    let bool: Token.t t =
        map
            (fun str -> Token.Bool, str)
            (CP.string "true" </> CP.string "false" <?> "bool")







    (* Combinator recognizing an arbitary token
     * ========================================
     *
     * Preceeding whitespace is stripped off and the token is equipped with its
     * start position and its end position.
     *)

    let token: Token_plus.t t =
        lexer
            whitespace
            (Token.End, "")
            (
                (* None of the tokens needs any backtracking, because all can be
                 * recognized by looking at the first character. *)
                number
                </> string
                </> bool
                </> lbrace
                </> rbrace
                </> lbrack
                </> rbrack
                </> comma
                </> colon
            )






    (* The final lexer
     * ===============
     *)

    module Parser =
    struct
        include CP.Parser

        let start: t =
            (* Lexer starting at the start of the input. *)
            make_partial Position.start () token

        let restart (lex: t): t =
            (* Restart the lexer at the current position and replay the not yet
             * consumed input on the restarted parser.
             *)
            assert (has_succeeded lex);
            assert (not (has_consumed_end lex));
            make_partial (position lex) () token |> transfer_lookahead lex
    end
end







(* Internal representation of a json construct
 * ===========================================
 *
 * It is a simplified json construct having as elementary values only strings,
 * integer numbers and booleans.
 *)


module Json =
struct
    type t =
        | Null
        | Number of int         (* make the tests simpler *)
        | String of string
        | Bool   of bool
        | List   of t list
        | Record of (string * t) list


    let number i = Number i

    let string s = String s

    let bool b   = Bool b

    let list lst = List lst

    let record lst = Record lst


    let rec to_string: t -> string =
        (* Compact string representation of a json value *)
        let open Printf
        in
        function
        | Null ->
            "null"

        | Number i ->
            sprintf "%d" i

        | Bool b ->
            sprintf "%b" b

        | String s ->
            let dquote = "\"" in
            dquote ^ s ^ dquote

        | List lst ->
            "["
            ^
            String.concat ", " (List.map to_string lst)
            ^
            "]"

        | Record lst ->
            "{"
            ^
            String.concat
                ", "
                (List.map
                     (fun (key, y) -> "\"" ^ key ^ "\": " ^ to_string y)
                    lst
                )
            ^
            "}"
end








(* The parser receiving lexical tokens and parsing a json construct
 * ================================================================
 *
 * Implemented as a [Token_parser] which can be used by the module
 * [Parse_with_lexer] to generate the final parser.
 *)


module Combinator =
struct
    module TP = Token_parser.Make (Unit) (Token) (Json) (Fmlib_std.Void)

    module Parser = TP.Parser

    open TP


    let const (a: 'a) (_: 'b): 'a =
        a

    let step
            (expect: string)
            (etp: Token.tp)
            (f: string -> 'a)
        : 'a t
        =
        TP.step
            expect
            (fun state _ (tp, str) ->
                 if tp = etp then
                     Some (f str, state)
                 else
                     None)

    let zero_or_more_separated (p: 'a t) (sep: 'b t): 'a list t =
        map
            List.rev
            (one_or_more_separated
                 (fun x -> return [x])
                 (fun lst _ x -> return (x :: lst))
                 p
                 sep)
        </>
        return []


    let string: string t =
        step "string" Token.String Fun.id

    let colon: _ t =
        step {|":"|} Token.Colon (const "")

    let comma: _ t =
        step {|","|} Token.Comma (const "")

    let lbrace: _ t =
        step {|"{"|} Token.Lbrace (const "")

    let rbrace: _ t =
        step {|"}"|} Token.Rbrace (const "")

    let lbrack: _ t =
        step {|"["|} Token.Lbrack (const "")

    let rbrack: _ t =
        step {|"]"|} Token.Rbrack (const "")

    let number: Json.t t =
        step "number" Token.Number (fun s -> Json.number (int_of_string s))

    let bool: Json.t t =
        step "bool" Token.Bool (fun s -> Json.bool (bool_of_string s))




    let rec json (): Json.t t =
        map Json.string string
        </>
        number
        </>
        bool
        </>
        (record () <?> "{ <key>: <value>, ... }")
        </>
        (list () <?> "[ <value>, ... ]")

    and record (): Json.t t =
        let* _     = lbrace in
        let* pairs =
            zero_or_more_separated
                (key_value_pair () <?> "<key>: <value>")
                comma
        in
        let* _     = rbrace in
        return Json.(Record pairs)

    and key_value_pair (): (string * Json.t) t =
        let* key = string in
        let* _   = colon  in
        let* value = json () in
        return (key, value)

    and list (): Json.t t =
        let* _   = lbrack in
        let* lst = zero_or_more_separated (json ()) comma in
        let* _   = rbrack in
        return (Json.List lst)

    let parse: Parser.t =
        make () (json ())

    let parse_partial: Parser.t =
        make_partial () (
            json ()
            </>
            expect_end Json.Null
        )
end








(* The complete parser
 * ===================
 *)


module Lex = Lexer.Parser

module Parse = Combinator.Parser


module Void = Fmlib_std.Void



module PL =
struct
    include Parse_with_lexer.Make (Unit) (Token) (Json) (Void) (Lex) (Parse)

    let start: t =
        make Lex.start Combinator.parse
end








(* Helper functions for unit tests and error reporting
 * ===================================================
 *)

module Pretty = Fmlib_pretty.Print

let write_error (str: string) (p: PL.t): unit =
    let module Reporter = Error_reporter.Make (PL) in
    if not (PL.has_succeeded p) then
        Reporter.(
            make_syntax p
            |> run_on_string str
            |> Pretty.layout 50
            |> Pretty.write_to_channel stdout
        )



let check_successes (arr: (string * string * string) array): bool =
    let check_success (tag, input, expected) =
        let open PL in
        let p = run_on_string input start in
        if not (has_succeeded p) then
            Printf.printf "unexpected failure of: %s\n" tag;
        write_error input p;
        has_succeeded p
        &&
        let res = final p |> Json.to_string in
        if res <> expected then
            Printf.printf "%s: expected %s, actual %s\n" tag expected res;
        res = expected
    in
    Array.for_all check_success arr




let check_failures (arr: (string * string * int * int * bool) array): bool =
    let check_failure (tag, input, row, col, flag) =
        let open PL in
        let p   = run_on_string input start in
        let pos = position p
        in
        if has_succeeded p then
            Printf.printf "unexpected success of test: %s\n" tag
        else if flag then
            write_error input p;

        has_failed_syntax p
        &&
        Position.line pos = row
        &&
        Position.column pos = col
    in
    Array.for_all check_failure arr
















(* Test cases
 * ==========
 *)


let success_cases: (string * string * string) array =
    [|
        "number", "100", "100";

        "bool", "  true", "true";

        "string", {|"hello"|}, {|"hello"|};

        "number list",
        "[100,   2,  1]", "[100, 2, 1]";

        "arbitrary list",
        {|[0, true , /**/ "hello", [ ], { }]|},
        {|[0, true, "hello", [], {}]|};

        "record",
        {|{  "a" :  1, "b"   : true  , "c": "hello"   }|},
        {|{"a": 1, "b": true, "c": "hello"}|};

        "empty list", "/**/ [ ] //", "[]";

        "complex",
        {|[ {}, [  1],   [  ], {"a": 0, "b": {  } }]|},
        {|[{}, [1], [], {"a": 0, "b": {}}]|};
    |]



let failure_cases: (string * string * int * int * bool) array =
    [|
        "nothing", "", 0, 0, false;

        "nothing with comment", "// comment", 0, 10, false;

        "unterminated multiline comment",
        "/*",
        0, 2, false;

        "missing comma", "[1 2]", 0, 3, false;

        "unterminated string", {| "|}, 0, 2, false;

        "unexpected additional json", "1 1", 0, 2, false;
    |]



let%test _ =
    check_successes success_cases


let%test _ =
    check_failures failure_cases











(* ============================================================
 * Partial Parsing
 * ============================================================
 *)

module Pwl_partial =
struct
    include Parse_with_lexer.Make (Unit) (Token) (Json) (Void) (Lex) (Parse)


    let start: t =
        make Lex.start Combinator.parse_partial

    let next (p: t): t =
        assert (has_succeeded p);
        assert (not (has_consumed_end p));
        make_next p Combinator.parse_partial


    let run_on_string (str: string): int * string list * t =
        let len = String.length str
        in
        let rec run i lst p =
            if PL.has_succeeded p && (i > len || PL.has_consumed_end p) then

                i, List.rev lst, p

            else if PL.has_succeeded p then

                let lst = (PL.final p |> Json.to_string) :: lst
                and p   = next p
                in
                run i lst p

            else if needs_more p then

                let i, p = run_on_string_at i str p in
                run i lst p

            else begin

                write_error str p;
                i, List.rev lst, p

            end
        in
        run 0 [] start

end

let%test _ =
    let str =
        {| 100 200 []  [{"a":{}}] {"a":10,"b" : [1,2,3]}|}
    and res = [
        "100"
      ; "200"
      ; "[]"
      ; {|[{"a": {}}]|}
      ; {|{"a": 10, "b": [1, 2, 3]}|}
    ]
    in
    let open Pwl_partial in
    let len       = String.length str
    and i, lst, p = run_on_string str
    in
    i = len + 1
    &&
    has_succeeded p
    &&
    lst = res
