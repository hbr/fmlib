# Release 0.5.1

## Fmlib_parse

- Rework of lookahead access. Main function now `fold_lookahead`.

- Added interfaces `MINIMAL_PARSER`, `NORMAL_PARSER`, `FULL_PARSER`, `LEXER`.

- Added `Token_parser` and `Parse_with_lex` to support the separation of parsing
  and lexing.



# Release 0.5.0

## Fmlib_std

- Performance improvement 'Array.replace'

## Fmlib_pretty

- Added function 'write_to_channel'

## Fmlib_parse

- Changed signature of repetition combinators (e.g. 'one_or_more' and
  'zero_or_more').

- Added a generic parsing combinator for parsing of operator expressions.

- Complete rework of layout parsing.

- Added functions to pretty print error messages and code snippets containing an
  error.

- Documentation for parsing improved and augmented significantly.
