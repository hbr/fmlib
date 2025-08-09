module type URL =
sig
    (** Construct and parse URLs. *)


    (** {1 Basics} *)

    (** The protocol (a.k.a. scheme) of the URL. Only web-related protocols are
        supported. *)
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
    (** The parts of a URL.

        The {{: https://tools.ietf.org/html/rfc3986 } URL spec} defines the
        parts like this:
        {[
              https://example.com:8042/over/there?name=ferret#nose
              \___/   \______________/\_________/ \_________/ \__/
                |            |            |            |        |
              scheme     authority       path        query   fragment
        ]}

        The strings in this type are not transformed in any way yet. The
        {!Parser} module has functions for that, e.g. splitting the path into
        segments, accessing specific query parameters and decoding the results
        into custom data types.

        Other parts of this library like {!Command.http_request} require URLs as
        strings. Those should be constructed directly using functions from the
        {!Builder} module, e.g {!Builder.absolute} or {!Builder.custom}.

        NOTE: Not all URL features from the URL spec are supported. For example,
        the [userinfo] segment as part of the [authority] is not supported.
    *)


    val of_string: string -> t option
    (** [of_string s] tries to split [s] into its URL parts.

        Examples:

        {[
            of_string "http://example.com:443"
            =
            Some
                {
                    protocol = Http;
                    host = "example.com";
                    port = Some 443;
                    path = "/";
                    query = None;
                    fragment = None;
                }

            of_string "https://example.com/hats?q=top%20hat"
            =
            Some
                {
                    protocol = Https;
                    host: "example.com";
                    port = None;
                    path = "/hats";
                    query = Some "q=top%20hat";
                    fragment = None;
                }

            of_string "example.com:443" = None (* no protocol *)

            of_string "http://tom@example.com" = None (* userinfo not allowed *)

            of_string "http://#cats" = None (* no host *)
        ]}
    *)


    (** {1 Percent-encoding} *)

    val percent_encode_part: string -> string
    (** [percent_encode_part s] encodes a part of a URL by escaping special
        characters like [?], [/] or non-ASCII characters. This uses
        Javascript's {{: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent } encodeURIComponent}
        internally.

        Normally the {!Builder} module should be used for constructing URLs.
        This function is here for exceptional cases that require more
        control.
    *)


    val percent_decode_part: string -> string option
    (** [percent_decode-part s] decodes a part of a URL recovering special
        characters like [?], [/] or non-ASCII characters. This uses
        Javascript's {{: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent } decodeURIComponent}
        internally.

        Normally the {!Parser} module should be used for decoding URLs. This
        function is here for exceptional cases that require more control.
    *)


    (** {1 Builders and parsers }*)

    module Builder:
    sig
        (** Construct URLs. *)


        (** {1 Path builders } *)

        type t
        (** The path builder type *)


        val raw: string -> t
        (** [raw s] creates a URL path segment from string [s] without
            percent-encoding it.

            This is useful for including readable unicode graphemes in the URL
            path. See {!absolute} for examples.
        *)


        val string: string -> t
        (** [string s] creates a URL path segment from string [s].

            Special characters like [?], [/] or non-ASCII characters will be
            automatically escaped using percent-encoding. See {!absolute} for
            examples.
        *)


        val int: int -> t
        (** [int i] creates a URL path segment from the integer [i].

            See {!absolute} for examples.
        *)



        (** {1 Query and fragment builders } *)

        module Query:
        sig
            (** Construct queries *)

            type t
            (** The query builder type *)


            val raw: string -> string -> t
            (** [raw key s] creates a query parameter with the given [key]
                and string value [s] without percent-encoding either.

                This is useful for including readable unicode graphemes in the
                URL query. See {!absolute} for examples.
            *)


            val string: string -> string -> t
            (** [string key s] creates a query parameter with the given [key]
                and string value [s]..

                Special characters in the key and in the value, like [?], [/] or
                non-ASCII characters will be automatically escaped using
                percent-encoding. See {!absolute} for examples.
            *)


            val int: string -> int -> t
            (** [int key i] creates a URL query parameter with the given [key]
                and integer value [i].

                See {!absolute} for examples.
            *)

        end


        module Fragment:
        sig
            (** Construct fragments *)

            type t
            (** The fragment builder type *)


            val raw: string -> t
            (** [raw s] creates a URL fragment from string [s] without
                percent-encoding it.

                This is useful for including readable unicode graphemes in the URL
                fragment.
            *)


            val string: string -> t
            (** [string s] creates a URL path segment from string [s].

                Special characters like [?], [/] or non-ASCII characters will be
                automatically escaped using percent-encoding.
            *)

        end


        (** {1 URL builders} *)

        (** The type of custom URL we want to construct using {!custom} *)
        type root =
            | Absolute
                (** A local URL starting with an absolute path. *)
            | Relative
                (** A local URL starting with a relative path (relative to the
                    current location). *)
            | Cross_origin of string
                (** [Cross_origin origin] is a remote URL where the given
                    [origin] is different from the current location. *)


        val absolute: t list -> Query.t list -> string
        (** [absolute path_segments query_parameters] constructs a local URL
            (omitting the scheme and authority parts), containing the given
            [path_segments] and [query_parameters].

            Examples:

            {[
                absolute [] []
                (* "/" *)

                absolute [string "blog"; int 2025; int 8; int 7] []
                (* "/blog/2025/8/7" *)

                absolute [string "products"] [Query.string "search" "hat"; Query.int "page" 2]
                (* "/products?search=hat&page=2" *)

                absolute [string "name"; string "Гельмут"] []
                (* "/name/%D0%93%D0%B5%D0%BB%D1%8C%D0%BC%D1%83%D1%82" *)

                absolute [string "name"; raw "Гельмут"] []
                (* "/name/Гельмут" *)

                absolute [] [Query.string "emoji" "😅"]
                (* "?emoji=%F0%9F%98%85" *)

                absolute [] [Query.raw "emoji" "😅"]
                (* "?emoji=😅" *)
            ]}
        *)


        val relative: t list -> Query.t list -> string
        (** [relative path_segments query_parameters] constructs a relative
            local URL (relative the the current location), containing the given
            [path_segments] and [query_parameters].

            This behaves the same as {!absolute}, but omits the leading slash.

            Examples:

            {[
                relative [] []
                (* "" *)

                relative [string "blog"; int 2025; int 8; int 7] []
                (* "blog/2025/8/7" *)
            ]}
        *)


        val cross_origin: string -> t list -> Query.t list -> string
        (** [cross_origin pre_path path_segments query_parameters] allows
            constructing a cross-origin URL, (a URL with a different host than
            the current location), containing the given [path_segments] and
            [query_parameters].

            [pre_path] is inserted before the path as-is without further checks
            or encoding.

            Example:

            {[
                cross_origin "https://ocaml.org" [string "books"] []
                (* "https://ocaml.org/books" *)
            ]}
        *)


        val custom: root -> t list -> Query.t list -> Fragment.t option -> string
        (** [custom root path_segments query_parameters fragment] allows
            constructing custom URLs containing the given [path_segments],
            [query_parameters] and [fragment].

            Examples:

            {[
                custom
                    Absolute
                    [string "article"; int 42]
                    [Query.string "search" "ocaml"]
                    [Some Fragment.string "conclusion"]
                (* "/article/42?search=ocaml#conclusion" *)

                custom
                    (Cross_origin "https://ocaml.org")
                    [string "excercises"]
                    []
                    (Some Fragment.int 6)
                (* "https://ocaml.org/excercises#6" *)
            ]}
        *)

    end


    module Parser:
    sig

        (** Parse URLs. *)



        (** {1 Basics } *)

        type url = t


        type ('a, 'b) t
        (** The URL parser type *)



        (** {1 Path} *)

        val string: (string -> 'a, 'a) t
        (** [string] will parse a segment of the path as string.

            The segment will be percent-decoded automatically.

            {t | input                              | parse result     |
               |------------------------------------|------------------|
               | ["http://host/alice/"]             | [Some "alice"]   |
               | ["http://host/bob"]                | [Some "bob"]     |
               | ["http://host/%D0%91%D0%BE%D0%B1"] | [Some "Боб"]     |
               | ["http://host/42/"]                | [Some "42"]      |
               | ["http://host/"]                   | [None]           |}
        *)


        val int: (int -> 'a, 'a) t
        (** [int] will parse a segment of the path as integer.

            {t | input                  | parse result    |
               |------------------------|-----------------|
               | ["http://host/alice/"] | [None]          |
               | ["http://host/bob"]    | [None]          |
               | ["http://host/42/"]    | [Some "42"]     |
               | ["http://host/"]       | [None]          |}
        *)


        val s: string -> ('a, 'a) t
        (** [s str] will parse a segment of the path if it matches the given
            string [str].

            The segment will be percent-decoded automatically.

            For example, the parser [s "blog" </> int] will behave as follows:

            {t | input                   | parse result    |
               |-------------------------|-----------------|
               | ["http://host/blog/42"] | [Some 42]       |
               | ["http://host/tree/42"] | [None]          |}
        *)


        val custom: (string -> 'a option) -> (('a -> 'b), 'b) t
        (** [custom f] will parse a segment of the path by applying the
            function [f] to the raw segment.

            Example:

            {[
                let positive_int: (int -> 'a, 'a) Parser.t =
                    Parser.custom @@
                        fun s ->
                            match int_of_string_opt s with
                            | Some i when i > 0 ->
                                Some i
                            | _ ->
                                None
            ]}

            The example parser produces the following results:

            {t | input               | parse result    |
               |---------------------|-----------------|
               | ["http://host/0"]   | [None]          |
               | ["http://host/-42"] | [None]          |
               | ["http://host/42"]  | [Some 42]       |}
        *)


        val (</>): ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
        (** [p1 </> p2] combines the segment parsers [p1] and [p2] and returns
            a new parser which will parse two path segments.

            Example:

            {[
                let blog: (int -> 'a, 'a) Parser.t =
                    let open Parser in
                    s "blog" </> int
            ]}

            The example parser will behave like this:

            {t | input                    | parse result    |
               |--------------------------|-----------------|
               | ["http://host/blog"]     | [None]          |
               | ["http://host/blog/42"]  | [Some 42]       |
               | ["http://host/blog/42/"] | [Some 42]       |}
        *)


        val map: 'a -> ('a, 'b) t -> (('b -> 'c), 'c) t
        (** [map f parser] transforms the [parser] via [f].

            [f] can be a function taking as many values as the parser
            produces (see example 1) or, in case the parser produces no values
            at all, [f] can be a variant constructor or a variant tag (see
            example 2).

            Examples 1:

            {[
                type date = {year: int; month: int; day: int}

                let date_parser: (date -> 'a, 'a) Parser.t =
                    let open Parser in
                    map
                        (fun year month day -> {year; month; day})
                        (int </> int </> int)
            ]}

            The parser in example 1 will produce the following results:

            {t | input                      | parse result                             |
               |----------------------------|------------------------------------------|
               | ["http://host/2025/08/"]   | [None]                                   |
               | ["http://host/2025/08/07"] | [Some {year = 2025; month = 8; day = 7}] |}

            Example 2:
            {[
                let language = Haskell | Ocaml | Rust

                let language_parser: (language -> 'a, 'a) Parser.t =
                    let open Parser in
                    one_of
                        [
                            map Haskell (s "hs");
                            map OCaml (s "ml");
                            map Rust (s "rs");
                        ]
            ]}

            The parser in example 2 will produce the following results:

            {t | input              | parse result   |
               |--------------------|----------------|
               | ["http://host/hs"] | [Some Haskell] |
               | ["http://host/ml"] | [Some OCaml]   |
               | ["http://host/rs"] | [Some Rust]    |
               | ["http://host/py"] | [None]         |}
        *)


        val one_of: ('a, 'b) t list -> ('a, 'b) t
        (** [one_of parsers] runs the given [parsers] in the order they are
            provided. The result is the result of the first succeeding parser or
            [None] if all of them fail.

            Example:
            {[
                type route =
                    | Index
                    | Article of int
                    | Comment of {id: int; article_id: int}

                let route_parser: (route -> 'a, 'a) Parser.t =
                    let open Parser in
                    one_of
                        [
                            map Index top;
                            map (fun id -> Article id) (s "blog" </> int);
                            map
                                (fun article_id id -> Comment {id; article_id})
                                (s "blog" </> int </> s "comment" </> int)
                        ]
            ]}

            The example parser will behave like this:

            {t | input                             | parse result                                |
               |-----------------------------------|---------------------------------------------|
               | ["http://host/"]                  | [Some Index]                                |
               | ["http://host/blog"]              | [None]                                      |
               | ["http://host/blog/42"]           | [Some (Article 42)]                         |
               | ["http://host/blog/42"]           | [Some (Article 42)]                         |
               | ["http://host/blog/42/comment"]   | [None]                                      |
               | ["http://host/blog/42/comment/5"] | [Some (Comment {id = 5; article_id = 42}))] |}
        *)


        val top: ('a, 'a) t
        (** [top] creates a parser that does not consume any path segment.

            It can be used together with {!one_of} in order to use a common
            prefix for multiple parsers:

            {[
                type route = Overview | Post of int

                let blog: (route -> 'a, 'a) Parser.t =
                    let open Parser in
                    s "blog" </>
                        one_of
                            [
                                map Overview top;
                                map (fun id -> Post id) (s "post" </> int);
                            ]
            ]}

            The example parser produces the following results:

            {t | input                        | parse result     |
               |------------------------------|------------------|
               | ["http://host/"]             | [None]           |
               | ["http://host/blog"]         | [Some Overview]  |
               | ["http://host/post/42"]      | [None]           |
               | ["http://host/blog/post/42"] | [Some (Post 42)] |}
        *)



        (** {1 Query} *)

        module Query:
        sig
            (** Parse query parameters *)



            (** {1 Parsers } *)

            type 'a t
            (** The query parser type *)


            val string: string -> (string option) t
            (** [string key] will parse the query parameter named [key] as string.

                Both the key and value of the query parameter will be
                percent-decoded automatically.

                For example. the parser [top </> Query.string "name"] will
                behave like this:

                {t | input                                   | parse result          |
                   |-----------------------------------------|-----------------------|
                   | ["http://host?name=Alice"]              | [Some (Some "Alice")] |
                   | ["http://host?name=Bob"]                | [Some (Some "Bob")]   |
                   | ["http://host?name=%D0%91%D0%BE%D0%B1"] | [Some (Some "Боб")]   |
                   | ["http://host?name="]                   | [Some (Some "")]      |
                   | ["http://host?name"]                    | [Some None]           |
                   | ["http://host"]                         | [Some None]           |}
            *)


            val int: string -> (int option) t
            (** [int key] will parse the query parameter named [key] as integer.

                The key of the query parameter will be percent-decoded
                automatically.

                For example. the parser [top </> Query.int "year"] will behave
                like this:

                {t | input                     | parse result       |
                   |---------------------------|--------------------|
                   | ["http://host?year=2025"] | [Some (Some 2025)] |
                   | ["http://host?year=Y2K"]  | [Some None]        |
                   | ["http://host?year="]     | [Some None]        |
                   | ["http://host?year"]      | [Some None]        |
                   | ["http://host"]           | [Some None]        |}
            *)


            val enum: string -> (string * 'a) list -> 'a option t
            (** [enum key table] will parse the query parameter named [key] as
                string and will try to transform this string into a value by
                looking it up in the given [table].

                Example:

                {[
                    let lang_parser: ([`Haskell | `OCaml | `Rust] option -> 'a, 'a) Parser.t =
                        let open Parser in
                        top <?>
                            Query.enum
                                "lang"
                                [("hs", `Haskell); ("ml", `OCaml); ("rs", `Rust)]
                ]}

                The example parser produces the following results:

                {t | input                   | parse result         |
                   |-------------------------|----------------------|
                   | ["http://host?lang=ml"] | [Some (Some `Ocaml)] |
                   | ["http://host?lang=py"] | [Some None]          |
                   | ["http://host?lang="]   | [Some None]          |
                   | ["http://host?lang"]    | [Some None]          |
                   | ["http://host"]         | [Some None]          |}
            *)


            val custom: string -> (string list -> 'a) -> 'a t
            (** [custom key f] will parse the query parameter named [key] by
                applying the function [f] to the list of raw (undecoded) string
                values referenced by [key] in the query string.

                While the other query parsers, {!string}, {!int} and {!enum},
                only allow at most one occurrence of a key in the query string,
                [custom] allows handling multiple occurrences.

                Example:

                {[
                    let posts: int list option Parser.t =
                        let open Parser in
                        top <?> custom "post" (List.filter_map int_of_string)
                ]}

                The example parser produces the following results:

                {t | input                         | parse result  |
                   |-------------------------------|---------------|
                   | ["http://host?post=2"]        | [Some [2]]    |
                   | ["http://host?post=2&post=7"] | [Some [2; 7]] |
                   | ["http://host?post=2&post=x"] | [Some [2]]    |
                   | ["http://host?hats=2"]        | [Some []]     |}
            *)


            (** {1 Mapping} *)

            val map: ('a -> 'b) -> 'a t -> 'b t
            (** [map f query_parser] transforms the [query_parser] which parses
                one query parameter via [f].

                Example:

                {[
                    let search_term_parser: ([`Search of string] -> 'a, 'a) Parser.t =
                        let open Parser in
                        top <?>
                            Query.map
                                (fun s -> `Search (Option.value ~default:"" s))
                                (Query.string "search")
                ]}

                The example parser produces the following results:

                {t | input                        | parse result             |
                   |------------------------------|--------------------------|
                   | ["http://host?search=ocaml"] | [Some (`Search "ocaml")] |
                   | ["http://host?search="]      | [Some (`Search "")]      |
                   | ["http://host?search"]       | [Some (`Search "")]      |
                   | ["http://host"]              | [Some (`Search "")]      |}
            *)

            val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
            (** [map f query_parser] transforms the [query_parser] which parses
                two query parameters via [f].

                Example:

                {[
                    type user = {fname: string option; lname: string option}

                    let user_parser: (user -> 'a, 'a) Parser.t =
                        let open Parser in
                        top <?>
                            Query.map2
                                (fun fname lname -> {fname; lname})
                                (Query.string "fname")
                                (Query.string "lname")
                ]}

                The example parser produces the following results:

                {t | input                                    | parse result                                           |
                   |------------------------------------------|--------------------------------------------------------|
                   | ["http://host?fname=Xavier&lname=Leroy"] | [Some ({fname = Some "Xavier"; lname = Some "Leroy"))] |
                   | ["http://host?fname=Xavier"]             | [Some ({fname = Some "Xavier"; lname = None})]         |
                   | ["http://host?"]                         | [Some ({fname = None; lname = None})]                                            |
                   | ["http://host"]                          | [Some ({fname = None; lname = None})]                                            |}
            *)

            val map3: ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
            (** [map f query_parser] transforms the [query_parser] which parses
                three query parameters via [f].
            *)


            val map4:
                ('a -> 'b -> 'c -> 'd -> 'e) ->
                'a t ->
                'b t ->
                'c t ->
                'd t ->
                'e t
            (** [map f query_parser] transforms the [query_parser] which parses
                four query parameters via [f].
            *)


            val map5:
                ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
                'a t ->
                'b t ->
                'c t ->
                'd t ->
                'e t ->
                'f t
            (** [map f query_parser] transforms the [query_parser] which parses
                five query parameters via [f].
            *)


            val map6:
                ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
                'a t ->
                'b t ->
                'c t ->
                'd t ->
                'e t ->
                'f t ->
                'g t
            (** [map f query_parser] transforms the [query_parser] which parses
                six query parameters via [f].
            *)


            val map7:
                ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
                'a t ->
                'b t ->
                'c t ->
                'd t ->
                'e t ->
                'f t ->
                'g t ->
                'h t
            (** [map f query_parser] transforms the [query_parser] which parses
                seven query parameters via [f].
            *)


            val map8:
                ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) ->
                'a t ->
                'b t ->
                'c t ->
                'd t ->
                'e t ->
                'f t ->
                'g t ->
                'h t ->
                'i t
            (** [map f query_parser] transforms the [query_parser] which parses
                eight query parameters via [f].
            *)

        end


        val (<?>): ('a, ('b -> 'c)) t -> 'b Query.t -> ('a, 'c) t
        (** [url_parser </?> query_parser] combines a [url_parser] with a
            [query_parser].

            For example, the parser [s "blog" <?> Query.string "search"]
            produces the following results:

            {t | input                        | parse result          |
               |------------------------------|-----------------------|
               | ["http://host/blog"]         | [Some None]           |
               | ["http://host?search=ocaml"] | [Some (Some "ocaml")] |
               | ["http://host?search="]      | [Some (Some "")]      |
               | ["http://host?search"]       | [Some (None)]         |}
        *)


        val query: 'a Query.t -> (('a -> 'b), 'b) t
        (** [query query_parser] converts [query_parser] to a URL parser.

            This is useful if a URL has an empty path and we want to parse
            query parameters.

            Example:

            {[
                (* The following parsers are equivalent *)

                let search_term_parser1: (string -> 'a, 'a) Parser.t =
                    let open Parser in
                    query (Query.string "search")

                let search_term_parser2: (string -> 'a, 'a) Parser.t =
                    let open Parser in
                    top <?> Query.string "search"
            ]}

            The example parsers behave as follows:

            {t | input                        | parse result          |
               |------------------------------|-----------------------|
               | ["http://host"]              | [Some None]           |
               | ["http://host?search=ocaml"] | [Some (Some "ocaml")] |
               | ["http://host?search="]      | [Some (Some "")]      |
               | ["http://host?search"]       | [Some (None)]         |}
        *)



        (** {1 Fragment} *)

        val fragment: (string option -> 'a) -> (('a -> 'b), 'b) t
        (** [fragment f] creates a fragment parser that produces a value by
            calling [f] on the fragment part of the URL.

            The fragment part is percent-decoded automatically.

            For example. the parser [s "excercises" </> fragment Fun.id]
            produces the folloing results:

            {t | input                        | parse result      |
               |------------------------------|-------------------|
               | ["http://host/excercises"]   | [Some None]       |
               | ["http://host/excercises#"]  | [Some (Some "")]  |
               | ["http://host/excercises#6"] | [Some (Some "6")] |}
        *)



        (** {1 Run parsers} *)

        val parse: (('a -> 'a), 'a) t -> url -> 'a option
        (** [parse parser url] runs the given [parser] on the given [url].

            Example:

            {[
                type route = Home | Blog of int | Not_found

                let route_parser: (route -> 'a, 'a) Parser.t =
                    let open Parser in
                    one_of
                        [
                            map Home top;
                            map (fun id -> Blog id) (s "blog" </> int);
                        ]

                let route_of_string (str: string): route =
                    match of_string str with
                    | None ->
                        Not_found
                    | Some url ->
                        Option.value ~default:Not_found (parse route_parser url)
            ]}

            The [route_of_string] function above produces the following results:

            {t | input                                        | parse result |
               |----------------------------------------------|--------------|
               | ["/blog/42"]                                 | [Not_found]  |
               | ["https://example.com/"]                     | [Home]       |
               | ["https://example.com/blog"]                 | [Not_found]  |
               | ["https://example.com/blog/42"]              | [Blog 42]    |
               | ["https://example.com/blog/42/"]             | [Blog 42]    |
               | ["https://example.com/blog/42#introduction"] | [Blog 42]    |
               | ["https://example.com/blog/42?search=ocaml"] | [Blog 42]    |
               | ["https://example.com/settings"]             | [Not_found]  |}
        *)

    end

end
