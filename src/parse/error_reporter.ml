module type FAILED_PARSER =
sig
    type t
    type expect = string * Indent.expectation option
    type semantic
    val has_failed_syntax: t -> bool
    val failed_expectations: t -> expect list
    val failed_semantic: t -> semantic
    val position: t -> Position.t
end



module Make (Parser: FAILED_PARSER) =
struct
    open Fmlib_pretty.Print


    type semantic = Parser.semantic

    type t = {
        p: Parser.t;
        semantic: semantic -> doc;
        extractor: Source_extractor.t;
    }

    let make
            (semantic_range: semantic -> Position.range)
            (semantic:       semantic -> doc)
            (p: Parser.t)
        : t
        =
        let open Parser in
        {
            p;
            semantic;
            extractor =
                if has_failed_syntax p then
                    Source_extractor.of_position 5 (position p)
                else
                    Source_extractor.of_range
                        5
                        (semantic_range (failed_semantic p));
        }


    let make_syntax (p: Parser.t): t =
        assert (Parser.has_failed_syntax p);
        let semantic _ = assert false
        in
        make semantic semantic p


    let needs_more (r: t): bool =
        Source_extractor.needs_more r.extractor

    let put (c: char) (r: t): t =
        {r with
         extractor = Source_extractor.put c r.extractor
        }

    let put_end (r: t): t =
        {r with
         extractor = Source_extractor.put_end r.extractor
        }

    let document (r: t): doc =
        let open Parser
        in
        Source_extractor.document r.extractor
        <+> cut <+>
        (
            if has_failed_syntax r.p then
                Syntax_error.document
                    (Position.column (position r.p))
                    (failed_expectations r.p)
            else
                r.semantic (failed_semantic r.p)
        )


    let run_on_stream
            (str: char Stream.t)
            (r: t)
        : doc
        =
        {r with extractor =
                    Source_extractor.run_on_stream str r.extractor
        }
        |> document


    let run_on_string (str: string) (r: t): Fmlib_pretty.Print.doc =
        run_on_stream
            (Stream.of_string str)
            r


    let run_on_channel (ic: in_channel) (r: t): Fmlib_pretty.Print.doc =
        run_on_stream
            (Stream.of_channel ic)
            r

    let run_on_channels
            (ic: in_channel)
            (width: int)
            (oc: out_channel)
            (r: t)
        : unit
        =
        assert (0 < width);
        let open Fmlib_pretty.Print
        in
        run_on_channel ic r
        |> layout width
        |> write_to_channel oc
end
