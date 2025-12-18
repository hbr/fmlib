module Dictionary = Fmlib_std.Btree.Map (String)

type protocol = Http | Https


type t =
    {
        protocol: protocol;
        host: string;
        port: int option;
        path: string;
        query: string option;
        fragment: string option;
    }


let of_string (s: string): t option =
    let take n s = String.sub s 0 n in
    let drop n s = String.sub s n (String.length s - n) in
    let chomp_before_path protocol fragment query path s =
        if String.length s = 0 || String.contains s '@' then
            None
        else
            match String.index_opt s ':' with
            | None ->
                Some {protocol; host = s; port = None; path; query; fragment}
            | Some i ->
                let host = take i s in
                let port = int_of_string_opt (drop (i + 1) s) in
                if String.length host > 0 && Option.is_some port then
                    Some {protocol; host; port; path; query; fragment}
                else
                    None
    in
    let chomp_before_query protocol fragment query s =
        match String.index_opt s '/' with
        | None ->
            chomp_before_path protocol fragment query "/" s
        | Some i ->
            let path = drop i s in
            let rest = take i s in
            chomp_before_path protocol fragment query path rest
    in
    let chomp_before_fragment protocol fragment s =
        match String.index_opt s '?' with
        | None ->
            chomp_before_query protocol fragment None s
        | Some i ->
            let query = Some (drop (i + 1) s) in
            let rest = take i s in
            chomp_before_query protocol fragment query rest
    in
    let chomp_after_protocol protocol s =
        match String.index_opt s '#' with
        | None ->
            chomp_before_fragment protocol None s
        | Some i ->
            let fragment = Some (drop (i + 1) s) in
            let rest = take i s in
            chomp_before_fragment protocol fragment rest
    in
    if String.starts_with ~prefix:"http://" s then
        chomp_after_protocol Http (drop 7 s)
    else if String.starts_with ~prefix:"https://" s then
        chomp_after_protocol Https (drop 8 s)
    else
        None


let to_string (url: t): string =
    let protocol =
        match url.protocol with
        | Http ->
            "http://"
        | Https ->
            "https://"
    in
    let port =
        match url.port with
        | None ->
            ""
        | Some p ->
            ":" ^ string_of_int p
    in
    let path = if url.path = "/" then "" else url.path in
    let query =
        match url.query with
        | None ->
            ""
        | Some q ->
            "?" ^ q
    in
    let fragment =
        match url.fragment with
        | None ->
            ""
        | Some f ->
            "#" ^ f
    in
    protocol ^ url.host ^ port ^ path ^ query ^ fragment


let percent_encode_part (s: string): string =
    Fmlib_js.Url.percent_encode_component s


let percent_decode_part (s: string): string option =
    Fmlib_js.Url.percent_decode_component s


module Builder =
struct

    type t = string


    let raw (segment: string): t =
        segment


    let string (segment: string): t =
        percent_encode_part segment


    let int (segment: int): t =
        string_of_int segment


    module Query =
    struct

        type t = string * string


        let raw (key: string) (value: string): t =
            (key, value)


        let string (key: string) (value: string): t =
            (percent_encode_part key, percent_encode_part value)


        let int (key: string) (value: int): t =
            (percent_encode_part key, string_of_int value)

    end


    module Fragment =
    struct

        type t = string

        let raw (x: string): t =
            x

        let string (x: string): t =
            percent_encode_part x

    end


    type root = Absolute | Relative | Cross_origin of string


    let encode_query (params: Query.t list): string =
        match params with
        | [] ->
            ""
        | params ->
            let string_of_pair (k, v) = k ^ "=" ^ v in
            "?" ^ (params |> List.map string_of_pair |> String.concat "&")


    let encode_path (path: t list): string =
        path |> String.concat "/"


    let relative (path: t list) (query: Query.t list): string =
        encode_path path ^ encode_query query


    let absolute (path: t list) (query: Query.t list): string =
        "/" ^ encode_path path ^ encode_query query


    let cross_origin (pre_path: string) (path: t list) (query: Query.t list): string =
        let path = if path = []  then "" else "/" ^ encode_path path in
        pre_path ^ path ^ encode_query query

    let custom (root: root) (path: t list) (query: Query.t list) (fragment: Fragment.t option): string =
        let fragment =
            match fragment with
            | None ->
                ""
            | Some f ->
                "#" ^ f
        in
        match root with
        | Absolute ->
            (absolute path query) ^ fragment
        | Relative ->
            (relative path query) ^ fragment
        | Cross_origin pre_path ->
            (cross_origin pre_path path query) ^ fragment

end


module Parser =
struct

    type 'a state =
        {
            unvisited: string list;
            params: string list Dictionary.t;
            fragment: string option;
            value: 'a;
        }


    type url = t


    type ('a, 'b) t = 'a state -> 'b state list


    let custom (func: string -> 'a option): (('a -> 'b), 'b) t =
        fun state ->
        match state.unvisited with
        | [] ->
            []
        | next :: unvisited ->
            begin
                match func next with
                | Some next_value ->
                    let value = state.value next_value in
                    [ {state with unvisited; value} ]
                | None ->
                    []
            end


    let string: (string -> 'a, 'a) t =
        fun x -> custom percent_decode_part x


    let int: (int -> 'a, 'a) t =
        fun x -> custom int_of_string_opt x


    let s_aux (s: string) (decode: string -> string option): ('a, 'a) t =
        fun state ->
        match state.unvisited with
        | [] ->
            []
        | next :: unvisited ->
            begin
                match decode next with
                | None ->
                    []
                | Some decoded ->
                    if decoded = s then
                        [ {state with unvisited } ]
                    else
                        []
            end


    let s (s: string): ('a, 'a) t =
        s_aux s percent_decode_part


    let (</>) (parser1: ('a, 'b) t) (parser2: ('b, 'c) t): ('a, 'c) t =
        fun state ->
        List.concat_map parser2 (parser1 state)


    let map (sub_value: 'a) (p: ('a, 'b) t): (('b -> 'c), 'c) t =
        let map_state f s =
            {s with value = f s.value}
        in
        fun state ->
        List.map (map_state state.value) (p {state with value = sub_value})


    let one_of (parsers: ('a, 'b) t list): ('a, 'b) t =
        fun state -> List.concat_map (fun p -> p state) parsers


    let top: ('a, 'a) t =
        fun state -> [state]


    module Query =
    struct

        type 'a t = string list Dictionary.t -> 'a


        let custom (key: string) (func: string list -> 'a): 'a t =
            fun dict ->
            func (Option.value ~default:[] (Dictionary.find_opt key dict))


        let string (key: string): (string option) t =
            let extract_result results =
                match results with
                | [result] ->
                    percent_decode_part result
                | _ ->
                    None
            in
            custom key extract_result


        let int (key: string): (int option) t =
            let extract_result results =
                match results with
                | [result] ->
                    int_of_string_opt result
                | _ ->
                    None
            in
            custom key extract_result


        let enum (key: string) (table: (string * 'a) list): 'a option t =
            let extract_result results =
                let dict = Dictionary.of_list table in
                match results with
                | [result] ->
                    begin
                        match percent_decode_part result with
                        | Some new_key ->
                            Dictionary.find_opt new_key dict
                        | None ->
                            None
                    end
                | _ ->
                    None
            in
            custom key extract_result


        let map (f: 'a -> 'b) (p: 'a t): 'b t =
            fun dict ->
            f (p dict)


        let return (a: 'a): 'a t =
            fun _ -> a


        let (<*>) (f: ('a -> 'b) t) (a: 'a t): 'b t =
            fun dict ->
            (f dict) (a dict)
    end



    let query (query_parser: 'a Query.t): (('a -> 'b), 'b) t =
        fun state ->
        let value = state.value (query_parser state.params) in
        [ {state with value} ]


    let (<?>) (parser: ('a, ('b -> 'c)) t) (query_parser: 'b Query.t): ('a, 'c) t =
        (</>) parser (query query_parser)


    let fragment (func: string option -> 'a): (('a -> 'b), 'b) t =
        fun state ->
        let fragment = Option.bind state.fragment percent_decode_part in
        let value = state.value (func fragment) in
        [ {state with value} ]


    let parse (parser: (('a -> 'a), 'a) t) (url: url): 'a option =
        let prepare_path (path: string): string list =
            let rec remove_final_empty segments =
                match segments with
                | [] ->
                    []
                | "" :: [] ->
                    []
                | segment :: rest ->
                    segment :: remove_final_empty rest
            in
            match String.split_on_char '/' path with
            | "" :: segments ->
                remove_final_empty segments
            | segments ->
                remove_final_empty segments
        in
        let prepare_query (query: string option): (string list) Dictionary.t =
            let add_param dict segment =
                match String.split_on_char '=' segment with
                | [raw_key; value] ->
                    begin
                        match percent_decode_part raw_key with
                        | Some key ->
                            Dictionary.update
                                key
                                (function
                                    | None -> Some [value]
                                    | Some lst -> Some (value :: lst))
                                dict
                        | None ->
                            dict
                    end
                | _ ->
                    dict
            in
            match query with
            | None ->
                Dictionary.empty
            | Some q ->
                List.fold_left
                    add_param
                    Dictionary.empty
                    (String.split_on_char '&' q)
        in
        let state =
            {
                unvisited = prepare_path url.path;
                params = prepare_query url.query;
                fragment = url.fragment;
                value = Fun.id;
            }
        in
        parser state
        |> List.find_opt (fun s -> s.unvisited = [] || s.unvisited = [""])
        |> Option.map (fun s -> s.value)

end


(* TESTS *)

let%test "round trip" =
    [
        "http://example";
        "http://example:8000/a?search=hat#chapter1";
    ]
    |> List.for_all
        (fun input -> input |> of_string |> Option.get |> to_string = input)


let%test "protocol" =
    [
        ("http://example", Http);
        ("https://example", Https);
    ]
    |> List.map (fun (input, proto) -> (Option.get (of_string input), proto))
    |> List.for_all (fun (url, proto) -> url.protocol = proto)


let%test "host" =
    [
        ("http://example", "example");
        ("http://приме́р", "приме́р");
        ("http://127.0.0.1", "127.0.0.1")
    ]
    |> List.map (fun (input, host) -> (Option.get (of_string input), host))
    |> List.for_all (fun (url, host) -> url.host = host)


let%test "port" =
    [
        ("http://example", None);
        ("http://example:443", Some 443);
        ("http://example:0", Some 0);
        ("http://example:65535", Some (65535));
    ]
    |> List.map (fun (input, port) -> (Option.get (of_string input), port))
    |> List.for_all (fun (url, port) -> url.port = port)


let%test "path" =
    [
        ("http://example", "/");
        ("http://example/", "/");
        ("http://example/a/b/c", "/a/b/c");
    ]
    |> List.map (fun (input, path) -> (Option.get (of_string input), path))
    |> List.for_all (fun (url, path) -> url.path = path)


let%test "query" =
    [
        ("http://example", None);
        ("http://example?", Some "");
        ("http://example?x=1&y=2", Some "x=1&y=2");
    ]
    |> List.map (fun (input, query) -> (Option.get (of_string input), query))
    |> List.for_all (fun (url, query) -> url.query = query)


let%test "fragment" =
    [
        ("http://example", None);
        ("http://example#", Some "");
        ("http://example#a", Some "a");
    ]
    |> List.map (fun (input, frag) -> (Option.get (of_string input), frag))
    |> List.for_all (fun (url, frag) -> url.fragment = frag)


let%test "invalid" =
    [
        "";
        "http://";
        "http:///";
        "http://?";
        "http://#";
        "http://:443";
        "test";
        "/test";
        "?test";
        "#test";
        "http://example:";
        "http://example:a";
        "http://user@example";
        "file://test.txt";
    ]
    |> List.map of_string
    |> List.for_all Option.is_none


let%test "absolute" =
    let open Builder in
    [
        (absolute [] [], "/");
        (absolute [string "a"; string ""; string "b"] [], "/a//b");
        (absolute [string "a"; int 123] [], "/a/123");
        (absolute [] [Query.string "x" "1"], "/?x=1");
        (absolute [] [Query.string "x" "1"; Query.int "y" 2], "/?x=1&y=2");
    ]
    |> List.for_all (fun (input, expected) -> input = expected)


let%test "relative" =
    let open Builder in
    [
        (relative [] [], "");
        (relative [string "a"; string ""; string "b"] [], "a//b");
        (relative [string "a"; int 123] [], "a/123");
        (relative [] [Query.string "x" "1"], "?x=1");
        (relative [] [Query.string "x" "1"; Query.int "y" 2], "?x=1&y=2");
    ]
    |> List.for_all (fun (input, expected) -> input = expected)


let%test "cross-origin" =
    let open Builder in
    [
        (cross_origin "http://example" [] [], "http://example");
        (cross_origin "http://example" [string "a"; string ""; string "b"] [], "http://example/a//b");
        (cross_origin "http://example" [string "a"; int 123] [], "http://example/a/123");
        (cross_origin "http://example" [] [Query.string "x" "1"], "http://example?x=1");
        (cross_origin "http://example" [] [Query.string "x" "1"; Query.int "y" 2], "http://example?x=1&y=2");
    ]
    |> List.for_all (fun (input, expected) -> input = expected)


let%test "custom" =
    let open Builder in
    [
        (
            custom Absolute [] [] None,
            "/"
        );
        (
            custom Absolute [string "a"; string "b"] [Query.string "a" "b"] (Some (Fragment.string "test")),
            "/a/b?a=b#test"
        );
        (
            custom Relative [] [] None,
            ""
        );
        (
            custom Relative [string "a"; string "b"] [Query.string "a" "b"] (Some (Fragment.string "test")),
            "a/b?a=b#test"
        );
        (
            custom (Cross_origin "http://example") [] [] None,
            "http://example"
        );
        (
            custom (Cross_origin "http://example") [string "a"; string "b"] [Query.string "a" "b"] (Some (Fragment.string "test")),
            "http://example/a/b?a=b#test"
        );
    ]
    |> List.for_all (fun (input, expected) -> input = expected)


let%test "percent-encode" =
    let open Builder in
    [
        (
            relative [string "a/b"] [],
            "a%2Fb"
        );
        (
            relative [string "имя"] [],
            "%D0%B8%D0%BC%D1%8F"
        );
        (
            relative [] [Query.string "a/b" "1/2"],
            "?a%2Fb=1%2F2"
        );
        (
            relative [] [Query.string "имя" "Гельмут"],
            "?%D0%B8%D0%BC%D1%8F=%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1%82"
        );
        (
            custom Relative [] [] (Some (Fragment.string "имя")),
            "#%D0%B8%D0%BC%D1%8F"
        );
    ]
    |> List.for_all (fun (input, expected) -> input = expected)


let%test "raw unicode" =
    let open Builder in
    [
        (relative [raw "имя"] [], "имя");
        (relative [] [Query.raw "имя" "Гельмут"], "?имя=Гельмут");
        (custom Relative [] [] (Some (Fragment.raw "имя")), "#имя");
    ]
    |> List.for_all (fun (input, expected) -> input = expected)


let%test "path parsing" =
    let open Parser in
    let url = Option.get (of_string "http://example/test/123") in
    [
        (
            map (fun s n -> `String_and_number (s, n)) (string </> int),
            Some (`String_and_number ("test", 123))
        );
        (
            map (fun n s -> `String_and_number (s, n)) (int </> string),
            None
        );
        (
            map (fun s -> `String s) (string),
            None
        );
        (
            map (fun n -> `Number n) (s "test" </> int),
            Some (`Number 123)
        );
        (
            map (fun n -> `Number n) (s "example" </> int),
            None
        );
    ]
    |> List.for_all (fun (parser, expected) -> parse parser url = expected)


let%test "advanced path parsing" =
    let open Parser in
    let url = Option.get (of_string "http://example/test/123") in
    [
        (
            map (fun s n -> `String_and_number (s, n)) (string </> int </> top),
            Some (`String_and_number ("test", 123))
        );
        (
            map (fun s -> `String s) (string </> top),
            None
        );
        (
            map (fun n -> `Number n) (one_of [s "example"; s "test"] </> int),
            Some (`Number 123)
        );
        (
            map (fun n -> `Number n) (one_of [s "example1"; s "example2"] </> int),
            None
        );
        (
            let nonempty_string s = if String.length s > 0 then Some s else None in
            map (fun s n -> `String_and_number (s, n)) (custom nonempty_string </> int),
            Some (`String_and_number ("test", 123))
        );
        (
            let big_int s =
                match int_of_string_opt s with
                | Some n when n > 123 ->
                    Some n
                | _ ->
                    None
            in
            map (fun s n -> `String_and_number (s, n)) (string </> custom big_int),
            None
        );
    ]
    |> List.for_all (fun (parser, expected) -> parse parser url = expected)


let%test "unicode path parsing" =
    let open Parser in
    [
        (
            "http://example/%D0%B8%D0%BC%D1%8F",
            map `Name (s "имя"),
            Some `Name
        );
        (
            "http://example/%D0%B8%D0%BC%D1%8F",
            map (fun s -> `Other s) string,
            Some (`Other "имя")
        );
        (
            "http://example/%D0%B8%D0%BC%D1",
            map (fun s -> `Other s) string,
            None
        );
        (
            "http://example/имя",
            map `Name (s "имя"),
            Some `Name
        );
        (
            "http://example/имя",
            map (fun s -> `Other s) string,
            Some (`Other "имя")
        );
    ]
    |> List.for_all
        (fun (input, parser, expected) ->
            parse parser (Option.get (of_string input)) = expected)


let%test "query parsing" =
    let open Parser in
    let url = Option.get (of_string "http://example?a=test&b=123") in
    [
        (
            top <?> Query.map (fun s -> `String s) (Query.string "a"),
            Some (`String (Some "test"))
        );
        (
            top <?> Query.map (fun n -> `Number n) (Query.int "b"),
            Some (`Number (Some 123))
        );
        (
            top <?> Query.map (fun s -> `String s) (Query.string "c"),
            Some (`String None)
        );
        (
            top
            <?>
            Query.(
                return (fun s n -> `String_and_number (s, n))
                <*> string "a"
                <*> int "b"
            ),
            Some (`String_and_number (Some "test", Some 123))
        );
        (
            top
            <?>
            Query.(
                return (fun n s -> `String_and_number (s, n))
                <*> int "b"
                <*> string "a"
            ),
            Some (`String_and_number (Some "test", Some 123))
        );
    ]
    |> List.for_all (fun (parser, expected) -> parse parser url = expected)


let%test "advanced query parsing" =
    let open Parser in
    let url = Option.get (of_string "http://example?name=Xavier&year=1990&year=1996") in
    [
        (
            let years strs =
                strs
                |> List.filter_map int_of_string_opt
                |> List.sort compare
            in
            top <?> Query.map (fun xs -> `Years xs) (Query.custom "year" years),
            Some (`Years [1990; 1996])
        );
        (
            let table = [("Xavier", `OCaml); ("Simon", `Haskell)] in
            top <?> Query.map (fun l -> `Language l) (Query.enum "name" table),
            Some (`Language (Some `OCaml))
        );
    ]
    |> List.for_all (fun (parser, expected) -> parse parser url = expected)


let%test "unicode query parsing" =
    let open Parser in
    [
        (
            "http://example?%D0%B8%D0%BC%D1%8F=%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1%82",
            top <?> Query.string "имя",
            Some (Some "Гельмут")
        );
        (
            "http://example?%D0%B8%D0%BC%D1=%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1%82",
            top <?> Query.string "имя",
            Some None
        );
        (
            "http://example?%D0%B8%D0%BC%D1%8F=%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1",
            top <?> Query.string "имя",
            Some None
        );
        (
            "http://example?имя=%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1%82",
            top <?> Query.string "имя",
            Some (Some "Гельмут")
        );
        (
            "http://example?%D0%B8%D0%BC%D1%8F=Гельмут",
            top <?> Query.string "имя",
            Some (Some "Гельмут")
        );
        (
            "http://example?имя=Гельмут",
            top <?> Query.string "имя",
            Some (Some "Гельмут")
        );
        (
            "http://example?%D0%B8%D0%BC%D1%8F=%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1%82",
            top <?> Query.enum "имя" [("Гельмут", "OCaml"); ("Simon", "Haskell")],
            Some (Some "OCaml")
        );
        (
            "http://example?имя=Гельмут",
            top <?> Query.enum "имя" [("Гельмут", "OCaml"); ("Simon", "Haskell")],
            Some (Some "OCaml")
        );
    ]
    |> List.for_all
        (fun (input, parser, expected) ->
            parse parser (Option.get (of_string input)) = expected)


let%test "fragment parsing" =
    let open Parser in
    [
        (
            "http://example",
            top </> fragment Fun.id,
            Some (None)
        );
        (
            "http://example#",
            top </> fragment Fun.id,
            Some (Some "")
        );
        (
            "http://example#test",
            top </> fragment Fun.id,
            Some (Some "test")
        );
    ]
    |> List.for_all
        (fun (input, parser, expected) ->
            parse parser (Option.get (of_string input)) = expected)


let%test "unicode fragment parsing" =
    let open Parser in
    [
        (
            "http://example#%D0%B8%D0%BC%D1%8F",
            top </> fragment Fun.id,
            Some (Some "имя")
        );
        (
            "http://example#%D0%B8%D0%BC%D1",
            top </> fragment Fun.id,
            Some None
        );
        (
            "http://example#имя",
            top </> fragment Fun.id,
            Some (Some "имя")
        );
    ]
    |> List.for_all
        (fun (input, parser, expected) ->
            parse parser (Option.get (of_string input)) = expected)
