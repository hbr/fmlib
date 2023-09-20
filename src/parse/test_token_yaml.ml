module Yaml =
struct
    type t =
        | Scalar of string
        | List   of t list
        | Record of (string * t) list


    let scalar str = Scalar str

    let list lst = List lst

    let record lst = Record lst

    module Pretty = Fmlib_pretty.Print

    let rec to_doc: t -> Pretty.doc = function
        | Scalar str ->
            Pretty.text str
        | List lst ->
            let open Pretty in
            lst
            |> List.map
                (fun y ->
                     text "- " <+> nest 2 (to_doc y))
            |> separated_by cut
        | Record lst ->
            let open Pretty in
            lst
            |> List.map
                   (fun (str,y) ->
                     text str <+> char ':'
                     <+> cut
                     <+> nest 4 (to_doc y))
            |> separated_by cut

    let to_string (str: string): string =
        String.(make 1 '"' ^ escaped str ^ make 1 '"')

    let rec to_json: t -> string = function
        | Scalar str ->
            to_string str
        | List lst ->
            "["
            ^
            String.concat ", " (List.map to_json lst)
            ^
            "]"
        | Record lst ->
            "{"
            ^
            String.concat
                ", "
                (List.map
                    (fun (key, y) -> to_string key ^ ": " ^ to_json y)
                    lst
                )
            ^
            "}"

    let write_to_channel (oc: out_channel) (y: t): unit =
        let open Pretty in
        to_doc y <+> cut
        |> layout 80
        |> write_to_channel oc
end



module Token =
struct
    type t =
        | String of string
        | Colon
        | Dash
        | End

    let string str = String str

    let to_string: t -> string = function
        | String str -> "<" ^ str ^ ">"
        | Colon -> "':'"
        | Dash -> "'-'"
        | End -> "end of input"
end


module Token_plus =
struct
    type t = Position.range * Token.t
end





module Lexer =
struct
    module CP = Character.Make (Unit) (Token_plus) (Fmlib_std.Void)

    open CP

    let comment: char t =
        let* _ = char '#' in
        let* _ =
            (charp (fun c -> c <> '\n') "comment character")
            |> skip_zero_or_more
        in
        return '#'


    let whitespace: int t =
        char ' ' </> char '\n' </> comment
        |> skip_zero_or_more
        |> no_expectations
        |> detach


    let colon: Token.t t =
        let* _ = char ':' in
        return Token.Colon

    let dash: Token.t t =
        let* _ = char '-' in
        return Token.Dash


    let raw_string: Token.t t =
        let expect  = "chars not containing colon and newline" in
        let inner c = c <> '\n' && c <> ':' && c <> '#' in
        let first c = c <> '"' && inner c
        in
        (word first inner expect)
        |> map (fun str -> String.trim str |> Token.string)


    let quoted_string: Token.t t =
        let expect = "chars except newline and dquote" in
        let ok c = c <> '\n' && c <> '"'
        in
        let* _   = char '"' in
        let* str = (word ok ok expect) </> return "" in
        let* _   = char '"'
        in
        return (Token.string str)



    let token: Token_plus.t t =
        lexer
            whitespace
            Token.End
            (
                colon
                </> dash
                </> raw_string
                </> quoted_string
            )


    module Parser =
    struct
        include CP.Parser

        let init: t =
            make_partial () token

        let restart (lex: t): t =
            restart_partial token lex
    end
end


module Combinator =
struct
    module TP = Token_parser.Make (Unit) (Token) (Yaml) (Fmlib_std.Void)

    module Parser = TP.Parser

    open TP

    let string: string t =
        step
            "string"
            (fun _ _ tok ->
                 match tok with
                 | Token.String str ->
                     Some (str, ())
                 | _ ->
                     None
            )

    let colon: char t =
        step
            "':'"
            (fun _ _ tok ->
                 match tok with
                 | Token.Colon ->
                     Some (':', ())
                 | _ ->
                     None
            )


    let dash: char t =
        step
            "'-'"
            (fun _ _ tok ->
                 match tok with
                 | Token.Dash ->
                     Some ('-', ())
                 | _ ->
                     None
            )

    let scalar: Yaml.t t =
        map Yaml.scalar string


    module Backtrack = struct
        let rec yaml (): Yaml.t t =
            sequence_block () </> record_block () </> scalar

        and sequence_block (): Yaml.t t =
            one_or_more
                (
                    sequence_element ()
                    <?>
                    "sequence element: \"- xxxx\""
                    |> align
                )
            <?> "sequence of aligned \"- xxxx\""
            |> map (fun (a, lst) -> Yaml.list (a :: lst))

        and sequence_element (): Yaml.t t =
            let* _ = dash in
            yaml () |> indent 1

        and record_block (): Yaml.t t =
            one_or_more
                (
                    record_element ()
                    <?> "key value pair: \"xxx: yyyy\""
                    |> align
                )
            <?> "sequence of aligned \"xxx: yyyy\""
            |> map (fun (a, lst) -> Yaml.record (a :: lst))

        and record_element (): (string * Yaml.t) t =
            let* str =
                backtrack
                    (
                        let* str = string in
                        let* _   = colon in
                        return str
                    )
                    "\"xxx:\""
            in
            let* y =
                (sequence_block () |> indent 0)
                </>
                (record_block () </> scalar |> indent 1)
            in
            return (str, y)


        let parse: Parser.t =
            make () (yaml ())
    end (* Backtrack *)


    module Left_factored = struct
        let rec yaml (): Yaml.t t =
            sequence_block () </> record_block_or_scalar ()


        and value_in_record (): Yaml.t t =
            (sequence_block () |> indent 0)
            </>
            (record_block_or_scalar () |> indent 1)

        and sequence_block (): Yaml.t t =
            one_or_more
                (
                    sequence_element ()
                    <?>
                    "sequence element: \"- xxxx\""
                    |> align
                )
            <?> "sequence of aligned \"- xxxx\""
            |> map (fun (a, lst) -> Yaml.list (a :: lst))

        and sequence_element (): Yaml.t t =
            let* _ = dash in
            yaml () |> indent 1

        and record_block_or_scalar (): Yaml.t t =
            let* str, y =
                string_or_record_element () |> align
            in
            match y with
            | None ->
                Yaml.scalar str |> return
            | Some y ->
                let* lst =
                    zero_or_more
                        (
                            record_element ()
                            <?> "key value pair: \"xxx: yyyy\""
                            |> align
                        )
                in
                (str, y) :: lst |> Yaml.record |> return

        and string_or_record_element (): (string * Yaml.t option) t =
            let* str = string in
            (
                let* _ =
                    colon
                    <?>
                    {|second part of a key value pair ": <value>"|}
                in
                let* y = value_in_record () in
                return (str, Some y)
            )
            </>
            return (str, None)

        and record_element (): (string * Yaml.t) t =
            let* str = string in
            let* _   = colon in
            let* y   = value_in_record () in
            return (str, y)


        let parse: Parser.t =
            make () (yaml ())
    end (* Left_factored *)

    let left_factor = true

    let parse =
        if left_factor then
            Left_factored.parse
        else
            Backtrack.parse
end


module Lex = Lexer.Parser

module Parse = Combinator.Parser


module Void = Fmlib_std.Void

module PL = struct
    include Parse_with_lexer.Make (Unit) (Token) (Yaml) (Void) (Lex) (Parse)

    let start: t =
        make Lex.init Combinator.parse
end

module Pretty = Fmlib_pretty.Print

let write_error (str: string) (p: PL.t): unit =
    let module Reporter = Error_reporter.Make (PL) in
    if PL.has_failed_syntax p then
        Reporter.(
            make_syntax p
            |> run_on_string str
            |> Pretty.layout 50
            |> Pretty.write_to_channel stdout
        )


let%test _ =
    let open PL in
    let str = {|
        names: - Alice
               - "Bob:#"
        category: encryption
        |}
    in
    let p = run_on_string str start in
    write_error str p;
    has_succeeded p
    &&
    let res = final p |> Yaml.to_json
    in
    let exp =
        {|{"names": ["Alice", "Bob:#"], "category": "encryption"}|}
    in
    res = exp




let%test _ =
    let open PL in
    let str = {|
        names: - Alice
               - "-Bob"
          category: encryption
        |}
    in
    let p = run_on_string str start in
    (*write_error str p;*)
    not (has_succeeded p)
