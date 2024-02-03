module type ANY = Fmlib_std.Interfaces.ANY
module Void     = Fmlib_std.Void


module Make (Final: ANY) =
struct
    include Ucharacter.Make_utf8 (Unit) (Final) (Void)



    (* Helper Functions
       ============================================================
    *)

    let is_ascii_letter (uc: Uchar.t): bool =
        let i = Uchar.to_int uc in
        (Char.code 'A' <= i && i <= Char.code 'Z')
        ||
        (Char.code 'a' <= i && i <= Char.code 'z')

    let is_greek_letter (uc: Uchar.t): bool =
        let i = Uchar.to_int uc in
        (0x03B1 <= i && i <= 0x03C9)
        ||
        (0x0391 <= i && i <= 0x03A9)

    let is_digit (uc: Uchar.t): bool =
        let i = Uchar.to_int uc in
        (Char.code '0' <= i && i <= Char.code '9')





    (* Whitespace
       ============================================================
    *)

    let ws_char: unit t =
        map
            (fun _ -> ())
            (char ' ' </> char '\t' </> char '\n' </> char '\r')

    let ws_uchar: unit t =
        map
            (fun _ -> ())
            (ucharp (fun uc -> uc = Uchar.bom) "Byte order mark")

    let line_comment: unit t =
        let* _ = backtrack (string "//") {|"//"|}
        in
        let* _ =
            skip_zero_or_more
                (ucharp
                     (fun uc -> Uchar.to_int uc <> Char.code '\n')
                     "any character except newline")
        in
        return ()

    let whitespace: int t =
        skip_zero_or_more
            (ws_char </> ws_uchar </> line_comment)
        |> no_expectations



    (* Lexeme
       ============================================================
    *)


    let lexeme (p: 'a t): 'a t =
        let* a = p
        in
        let* _ = whitespace
        in
        return a




    (* Grammar
       ============================================================
    *)
    let identifier: string t =
        uword
            (fun uc -> is_ascii_letter uc || is_greek_letter uc)
            (fun uc -> is_ascii_letter uc || is_greek_letter uc || is_digit uc)
            "identifier"
end


module Id_list =
struct
    include Make (struct type t = string list end)

    let id_list: string list t =
        let* _ = whitespace
        in
        zero_or_more (lexeme identifier)


    let make: Parser.t =
        make () id_list


    let write_error (flg: bool) (input: string) (p: Parser.t): unit =
        let module Reporter = Error_reporter.Make (Parser) in
        let module Pretty   = Fmlib_pretty.Print in
        if Parser.(flg && has_ended p && not (has_succeeded p)) then
            Reporter.(
                make_syntax p
                |> run_on_string input
                |> Pretty.layout 50
                |> Pretty.write_to_channel stdout
            )


    let run_on_string_error
            (flg: bool) (input: string) (p: Parser.t)
        : Parser.t
        =
        let p = Parser.run_on_string input p in
        write_error flg input p;
        p
end








(* Unit Tests
   ============================================================
*)


open Printf

let alpha = "α"
let beta  = "β"
let gamma = "γ"
let bom   = "\u{FEFF}"


let%test _ =
    let open Id_list in
    let open Parser in
    let input = sprintf " %s %s"  "\xFF" "\x08" in
    let p = run_on_string_error false input make
    in
    not (has_succeeded p)
    && line p = 0
    && column p = 1



let%test _ =
    let open Id_list in
    let open Parser in
    let input = sprintf " x%s1 %s \n //   \n y%s2 %sz3" alpha bom beta gamma
    in
    let p = run_on_string_error true input make
    in
    has_succeeded p
    && line p = 2
    && column p = 8
    && final p = [
        sprintf "x%s1" alpha
      ; sprintf "y%s2" beta
      ; sprintf "%sz3" gamma
    ]
