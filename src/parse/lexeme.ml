open Lexeme_intf



module Make: MAKE = functor  (Combi: COMBI) (L: LANG) ->
struct



    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        Basics
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)

    open Combi

    module String_set = Fmlib_std.Btree.Set (String)

    module String_map = Fmlib_std.Btree.Map (String)

    let set_of_list (lst: string list): String_set.t =
        List.fold_left
            (fun set name -> String_set.add name set)
            String_set.empty
            lst


    let reserved_names: String_set.t =
        set_of_list L.reserved_names


    let optional_with_default (d: 'a) (p: 'a t): 'a t =
        optional p
        |>
        map (function
            | None -> d
            | Some a -> a)







    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        Whitespace and Comments
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)



    let line_comment (start: string): int t =
        assert (start <> "");
        let* _ = backtrack (string start) "line comment" in
        let* n =
            charp (fun c ->
                c <> '\n')
                "all characters except newline"
            |> skip_zero_or_more
        in
        return (String.length start + n)



    let multiline_comment (start: string) (end_: string) (nested: bool): int t =
        assert (start <> "");
        assert (end_  <> "");
        assert (start <> end_);
        let len_start = String.length start
        and len_end   = String.length end_
        in
        let find_start =
            let* _ = backtrack (string start) start
            in
            return len_start
        in
        let rec skip n i level =
            let next_char =
                let* c = charp (fun _ -> true) "any char" in
                if c = end_.[i] then
                    skip (n + 1) (i + 1) level
                else
                    skip (n + 1) i level
            and next_level =
                let* m = find_start in
                skip (n + m) 0 (level + 1)
            in
            if level = 0 && i = len_end then
                return n

            else if i = len_end then
                skip n 0 (level - 1)

            else if nested then
                next_level </> next_char

            else
                next_char
        in
        let* n = find_start in
        skip n 0 0


    let whitespace =
        let whitespace_char: int t =
            one_of_chars L.whitespace_chars "whitespace char"
            |> map (fun _ -> 1)
        in
        let whitespace_token: int t =
            match L.line_comment, L.multiline_comment with
            | None, None ->
                whitespace_char

            | Some start, None ->
                line_comment start
                </>
                whitespace_char

            | None, Some (mstart, mend, nested) ->
                multiline_comment mstart mend nested
                </>
                whitespace_char

            | Some lstart, Some (mstart, mend, nested) ->
                line_comment lstart
                </>
                multiline_comment mstart mend nested
                </>
                whitespace_char
        in
        zero_or_more_fold_left
            0
            (fun n m -> return (n + m))
            whitespace_token







    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        Lexeme Support
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)


    let whitespace_before (p: 'a t): 'a t =
        let* _ = whitespace in
        p


    let lexeme (p: 'a t): 'a t =
        let* a = p in
        let* _ = whitespace in
        return a




    let token (expect: string) (p: 'a t): 'a t =
        backtrack p expect
        |> lexeme








    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        Token
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)

    let comma: char t =
        lexeme (char ',')


    let semicol: char t =
        lexeme (char ';')


    let colon: char t =
        lexeme (char ':')


    let dot: char t =
        lexeme (char '.')



    let string (s: string): string Located.t t =
        lexeme (string s |> located)


    let digit_char: char t =
        charp (fun c -> '0' <= c && c <= '9') "digit"


    let digit: int t =
        let* d = digit_char
        in
        return Char.(code d - code '0')


    let sign (plus: 'a) (minus: 'a): 'a t =
        (char '+' |> map (fun _ -> plus))
        </>
        (char '-' |> map (fun _ -> minus))
        |> optional_with_default plus


    let unsigned_int_base: (int * int) t =
        one_or_more_fold_left
            (fun d -> return (1, d))
            (fun (n, v) d -> (n + 1, 10 * v + d) |> return)
            digit


    let signed_int_base: int t =
        let* s = sign 1 (-1) in
        map (fun (_, v) -> s * v) unsigned_int_base


    let unsigned_int: int Located.t t =
        backtrack unsigned_int_base "unsigned int"
        |> map snd
        |> located
        |> lexeme


    let int: int Located.t t =
        signed_int_base
        |> located
        |> lexeme




    let float: float Located.t t =
        let fraction =
            let* _ = char '.' in
            unsigned_int_base
            |> map (fun (n, v) ->
                float_of_int v
                /.
                10.0 ** float_of_int n)
        and exponent =
            let* _ = (char 'e' </> char 'E') in
            signed_int_base
            |> map (fun e -> 10.0 ** float_of_int e)
        in
        let* s = sign 1.0 (~-. 1.0) in
        let* v = map (fun (_, v) -> float_of_int v) unsigned_int_base in
        let* frac = optional_with_default 0.0 fraction in
        let* exp  = optional_with_default 1.0 exponent in
        return (s *. (v +. frac) *. exp)
        |> located
        |> lexeme



    let raw_word (start: char -> bool) (letter: char -> bool): string t =
        let* c0 = charp start "identifier start" in
        zero_or_more_fold_left
            (String.make 1 c0)
            (fun str c -> String.make 1 c ^ str |> return)
            (charp letter "identifier inner")


    let generic_word
            (start: char -> bool)
            (letter: char -> bool)
            (expected: string)
            (reserved: String_set.t)
        : string Located.t t
        =
        backtrack
            ( let* word = raw_word start letter in
              if String_set.mem word reserved then
                  unexpected expected
              else
                  return word)
            expected
        |> located
        |> lexeme


    let generic_reserved
            (start: char -> bool)
            (letter: char -> bool)
            (expected: string)
        : string Located.t t
        =
        backtrack
            ( let* word = raw_word start letter in
              if word = expected then
                  return word
              else
                  unexpected expected)
            expected
        |> located
        |> lexeme



    let identifier: string Located.t t =
        generic_word
            L.identifier_start
            L.identifier_inner
            "identifier"
            reserved_names


    let reserved (expected: string): string Located.t t =
        generic_reserved
            L.identifier_start
            L.identifier_inner
            expected








    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        Parenthesized Expressions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)

    let generic_parens c1 c2 p =
        let* _ = char c1 |> lexeme in
        let* a = p () in
        let* _ = char c2|> lexeme in
        return a

    let parens (p: unit -> 'a t): 'a t =
        generic_parens '(' ')' p


    let brackets (p: unit -> 'a t): 'a t =
        generic_parens '[' ']' p


    let braces (p: unit -> 'a t): 'a t =
        generic_parens '{' '}' p


    let angulars (p: unit -> 'a t): 'a t =
        generic_parens '<' '>' p




    (*
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        Operator Expressions
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    *)

    type assoc = Left | Right

    type 'e unary_operation  =
        Position.range -> 'e Located.t -> 'e t

    type 'e binary_operation =
        'e Located.t -> Position.range -> 'e Located.t -> 'e t

    type 'e operation =
        | Unary  of 'e unary_operation
        | Binary of 'e binary_operation
        | Both   of 'e unary_operation * 'e binary_operation

    type 'e operator_table =
        (string * assoc * 'e operation) list list


    let lift_unary (op: 'e -> 'e): 'e unary_operation =
        fun _ (_, v) -> return (op v)


    let lift_binary (op: 'e -> 'e -> 'e) =
        fun (_, v1) _ (_, v2) -> return (op v1 v2)


    let expression
            (operator: string t)
            (prim: (unit -> 'e Located.t t) -> 'e Located.t t)
            (table: 'e operator_table)
        : 'e Located.t t
        =
        let _, op_map =
            List.fold_right
                (fun ops (prec, map) ->
                     let map: (int * assoc * 'e operation) String_map.t =
                         List.fold_left
                             (fun map (name, assoc, op) ->
                                  String_map.update name
                                      (function
                                          | None -> Some (prec, assoc, op)
                                          | Some _ ->
                                              assert false (* Illegal call,
                                                              duplicate operator
                                                              *)
                                      )
                                      map
                             )
                             map
                             ops
                     in
                     prec + 1, map)
                table
                (0, String_map.empty)
        in
        let unary_operator =
            Some (
                backtrack
                    ( let* range, op_string = located operator in
                      match String_map.find_opt op_string op_map with
                      | None ->
                          unexpected "operator"

                      | Some (_, _, Binary _) ->
                          unexpected "unary operator"

                      | Some op ->
                          return (range, op))
                    "unary operator"
                |> lexeme
            )

        and binary_operator =
            backtrack
                ( let* range, op_string = located operator in
                  match String_map.find_opt op_string op_map with
                  | None ->
                      unexpected "operator"

                  | Some (_, _, Unary _) ->
                      unexpected "binary operator"

                  | Some op ->
                      return (range, op))
                "binary operator"
            |> lexeme

        and is_left (_, (prec1, assoc1, _)) (_, (prec2, _, _)) =
            return (
                prec1 > prec2
                ||
                ( prec1 = prec2 && assoc1 = Left )
            )

        and make_unary
                (range_op, (_, _, op))
                ((range_e, _) as e)
            =
            let range = Position.merge range_op range_e
            in
            match op with
            | Unary op | Both (op, _) ->
                op range_op e
                |> map (fun e -> range, e)

            | Binary _ ->
                assert false (* cannot happen *)

        and make_binary
                ((range1, _) as e1)
                (range_op, (_, _, op))
                ((range2, _) as e2)
            =
            let range = Position.(
                merge (merge range1 range_op) range2
            )
            in
            match op with
            | Binary op | Both (_, op) ->
                op e1 range_op e2
                |> map (fun e -> range, e)

            | Unary _ ->
                assert false (* cannot happen *)
        in
        let rec exp (): 'e Located.t t =
            operator_expression
                (prim exp)
                unary_operator
                binary_operator
                is_left
                make_unary
                make_binary
        in
        exp ()

    let _ = expression
end
