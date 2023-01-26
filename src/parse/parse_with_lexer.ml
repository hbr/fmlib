module type ANY = Fmlib_std.Interfaces.ANY





module Make
        (State: ANY)
        (Token: ANY)
        (Final: ANY)
        (Semantic: ANY)
        (Lex: Interfaces.LEXER with type final = Position.range * Token.t)
        (Parse: Interfaces.FULL_PARSER with
                type state = State.t
            and type token = Position.range * Token.t
            and type expect= string * Indent.expectation option
            and type final = Final.t
            and type semantic = Semantic.t)
=
struct
    type token    = char
    type final    = Final.t
    type expect   = string * Indent.expectation option
    type semantic = Semantic.t
    type state    = State.t

    type t = {
        lex:   Lex.t;
        parse: Parse.t;
    }

    let make (lex: Lex.t) (parse: Parse.t): t =
        {lex; parse}


    let lex (p: t): Lex.t =
        p.lex


    let parse (p: t): Parse.t =
        p.parse


    let needs_more (p: t): bool =
        Lex.needs_more p.lex
        &&
        Parse.needs_more p.parse

    let has_succeeded (p: t): bool =
        Parse.has_succeeded p.parse


    let has_failed_syntax (p:t): bool =
        if Parse.needs_more p.parse then
            Lex.has_failed_syntax p.lex
        else
            Parse.has_failed_syntax p.parse


    let has_failed_semantic (p: t): bool =
        Parse.has_failed_semantic p.parse


    let final (p: t): Final.t =
        assert (has_succeeded p);
        Parse.final p.parse


    let failed_expectations
            (p: t)
        : expect list
        =
        assert (has_failed_syntax p);
        if Parse.needs_more p.parse then
            Lex.failed_expectations p.lex
        else
            Parse.failed_expectations p.parse


    let failed_semantic (p: t): Semantic.t =
        assert (has_failed_semantic p);
        Parse.failed_semantic p.parse



    let position (p: t): Position.t =
        match Parse.first_lookahead_token p.parse with
        | None ->
            Lex.position p.lex
        | Some ((p1, _), _) ->
            p1



    let state (p: t): State.t =
        Parse.state p.parse


    let rec check_token (p: t): t =
        if Lex.has_succeeded p.lex then
            check_token {
                lex =
                    Lex.restart p.lex;
                parse =
                    Parse.put (Lex.final p.lex) p.parse
            }
        else
            p


    let put (c: char) (p: t): t =
        check_token {p with lex = Lex.put c p.lex}

    let put_end (p: t): t =
        let p =
            check_token {p with lex = Lex.put_end p.lex}
        in
        assert (not (Lex.has_succeeded p.lex));
        match Lex.first_lookahead_token p.lex with
        | None ->
            {p with parse = Parse.put_end p.parse}
        | Some _ ->
            p


    let run_on_string = Run_on.string needs_more put put_end
end
