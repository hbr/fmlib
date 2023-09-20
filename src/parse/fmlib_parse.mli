(** Parsing Library *)


(** {1 Documentation}

    {{!page-parse} Introduction to Combinator Parsing}

*)


(** {1 Utilities} *)

module Position = Position

module Located  = Located

module Indent = Indent

module Error_reporter = Error_reporter

module Interfaces = Interfaces




(** {1 Parsers} *)



(** {2 Parse streams of characters}

    Character parsers are the simplest parsers. The tokens are characters. In
    order to generate a character parser you just need 3 modules. A [State]
    module which in many cases is just [Unit], a module [Final] to describe the
    type of the construct which the parser returns after successful parsing and
    a module [Semantic] which describes the semantic errors (the parser itself
    handles just syntax errors).
 *)

module Character = Character






(** {2 Parsing with lexers}

    Sometimes pure character parser are not very efficient if a lot of
    backtracking is necessary (and for many languages backtracking is
    necessary). Backtracking causes all characters of a failed construct to be
    pushed back into the lookahead and rescanning all characters for a
    different construct.

    For these cases the library offers parsers with 2 layers. A lexer and a
    token parser. The lexer parses the lexical tokens. A lexer usually needs no
    or very little backtracking. The token parser receives the already parsed
    tokens where each token is a unit consisting of all parsed characters. In
    case of backtracking the token parser just pushes back the whole tokens (not
    character by character) into the lookahead and reparses the whole tokens
    (again not character by character).

*)

module Token_parser = Token_parser

module Parse_with_lexer = Parse_with_lexer



(** {2 Full generic parser}

    All parsers of the library are based on this generic parser. The user
    usually does not write a generic parser.
*)

module Generic = Generic
