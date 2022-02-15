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


module CP = Character.Make (Unit) (Yaml) (Unit)

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


let lexeme (p: 'a t): 'a t =
    let* a = p in
    let* _ = whitespace in
    return a


let raw_string: string t =
    let expect  = "chars not containing colon and newline" in
    let inner c = c <> '\n' && c <> ':' && c <> '#' in
    let first c = c <> '"' && inner c
    in
    (word first inner expect)
    |> map String.trim
    |> lexeme

let quoted_string: string t =
    let expect = "chars except newline and dquote" in
    let ok c = c <> '\n' && c <> '"'
    in
    let* _   = char '"' in
    let* str = (word ok ok expect) </> return "" in
    let* _   = char '"' |> lexeme
    in
    return str

let scalar: Yaml.t t =
    quoted_string
    </>
    raw_string
    |>
    map Yaml.scalar
    <?>
    "scalar"

let dash: char t =
    char '-' |> lexeme

let key: string t =
    backtrack
        (
            let* str = raw_string </> quoted_string in
            let* _   = char ':' |> lexeme in
            return str
        )
        "<key>:"




let rec yaml (): Yaml.t t =
    sequence_block ()
    </>
    record_block ()
    </>
    scalar


and sequence_block (): Yaml.t t =
    one_or_more
        (
            sequence_element ()
            <?>
            "sequence element: \"- <yaml value>\""
            |> align
        )
    |> map (fun (a, lst) -> Yaml.list (a :: lst))
    <?> "sequence of aligned \"- <yaml value>\""


and record_block (): Yaml.t t =
    one_or_more
        (
            record_element ()
            <?> "key value pair: \"<key>: <yaml value>\""
            |> align
        )
    |> map (fun (a, lst) -> Yaml.record (a :: lst))
    <?> "sequence of aligned \"<key>: <yaml value>\""


and sequence_element (): Yaml.t t =
    let* _ = dash in
    yaml () |> indent 1


and record_element (): (string * Yaml.t) t =
    let* str = key in
    let* y   =
        sequence_block () |> indent 0
        </>
        (record_block () </> scalar |> indent 1)
    in
    return (str, y)




let parse: Parser.t =
    make
        ()
        (
            let* _ = whitespace in
            yaml ()
        )


module Pretty = Fmlib_pretty.Print



let write_errors (source: string) (p: Parser.t): unit =
    let open Parser in
    let open Error_reporter
    in
    assert (has_result p);
    assert (not (has_succeeded p));
    make_syntax p
    |> run_on_string source
    |> Pretty.layout 50
    |> Pretty.write_to_channel stdout


let write_result (source: string) (p: Parser.t): unit =
    let open Parser in
    if has_succeeded p then
        Yaml.(to_json (final p))
        |> print_endline
    else if has_failed_syntax p then
        write_errors source p
    else
        assert false (* Illegal call, no semantic errors *)


let print_expectations (p: Parser.t): unit =
    let open Printf in
    let open Parser in
    assert (has_failed_syntax p);
    Stdlib.List.iter
        (fun (str, ind) ->
             match ind with
             | None ->
                 printf "- %s\n" str
             | Some ind ->
                 match ind with
                 | Indent.Indent i ->
                     printf "- Indent %d: %s\n" i str
                 | Indent.Align i ->
                     printf "- Align %d: %s\n" i str
                 | Indent.Align_between (i, j) ->
                     printf "- Between %d %d: %s\n" i j str
        )
        (failed_expectations p)






(* Unit Tests
 * ==========
 *)



let%test _ =
    let str = "   hello  #comment"
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    final p = Yaml.scalar "hello"



let%test _ =
    let str = "- - hello  #comment"
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    Yaml.to_json (final p) = {|[["hello"]]|}



let%test _ =
    let str = "key : hello  "
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    Yaml.to_json (final p) = {|{"key": "hello"}|}



let%test _ =
    let str = "key:\n hello"
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    Yaml.to_json (final p) = {|{"key": "hello"}|}



let%test _ =
    let str = {|
        1: 11: hello  #comment"
           12: ""
           "###": hash
        |}
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    Yaml.to_json (final p)
    =
    {|{"1": {"11": "hello", "12": "", "###": "hash"}}|}





let%test _ =
    let str = {|
        -
         hello
        |}
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    Yaml.to_json (final p)
    = {|["hello"]|}





let%test _ =
    let str = {|
-
hello
        |}
    in
    let open Parser in
    let p = run_on_string str parse in
    has_failed_syntax p
    &&
    line p = 2
    &&
    column p = 0





let%test _ =
    let str = {|
"1:":- Alice # comment
  2: Bob
        |}
    in
    let open Parser in
    let p = run_on_string str parse in
    has_failed_syntax p
    &&
    (line p = 2)
    &&
    (column p = 2)




let%test _ =
    let str = {|
        k1:
        - 1
        - - 1.1
          - 1.2
        k2: s2
        |}
    in
    let open Parser in
    let p = run_on_string str parse in
    has_succeeded p
    &&
    Yaml.to_json (final p)
    =
    {|{"k1": ["1", ["1.1", "1.2"]], "k2": "s2"}|}
