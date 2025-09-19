Release 0.6.0
================================================================================

A lot of functionality has been added to the library. Unfortunately some changes
are breaking. It is quite easy to adapt to the changes, because only the
syntactic interface (different module names, function names, ...) has been
changed but not the semantic interface.

- `Fmlib_browser`:

    - More generic http requests.
    - Fix: focussing and blurring of elements
    - Added simple tasks to the module 'Command': It is now possible to execute
      simple tasks directly as a command without the need to make a task.
    - Reference nodes: It is possible to add reference elements into the dom.
      The reference nodes can be modified via commands. Reference nodes can be
      used to optimize html pages with many many elements (e.g. spreadsheets).
      The performance gain is in the range of jane street's incremental.
    - Local files can be selected a used within a web application.
    - Local and session storage implemented.
    - Added module 'Url' to parse and generate urls.
    - Added support for single page applications with access to the browser history.


- `Fmlib_pretty`:

    - Redesign based on Philip Wadler's design of a pretty printer. With the
      redesign some bugs have been fixed. The module 'Print' (old design) is
      marked as obsolete and will be removed in the next releases. The
      redesigned module is called 'Pretty' and has nearly the same interface as
      the old module.


- `Fmlib_parse`:

    - Added lexeme parsers.
    - Fix: Backtracking did not work correctly in some corner cases.




Release 0.5.9
================================================================================

Bugfix release to fix a bug in unicode parsers by parsing empty input streams.


Release 0.5.8
================================================================================

- `Fmlib_browser`:

    Added parallel execution of tasks.


- `Fmlib_parse`:

    Added unicode parsing

    Added `byte_position` and `byte_offset` to module `Position`

    Added and streamlined support for partial parsing. In particular the parsers
    with lexers support fully partial parsers. Glueing of partial parsers now
    works with `fold_lookahead` or `transfer_lookahead`.





Release 0.5.7
================================================================================

- `Fmlib_parse:`:

    Fix: Bug in `Source_extractor`, introduced in the previous release.

    Added `Parse_with_lexer.range` to return the range of the token which caused
    a syntax error.



Release 0.5.6
================================================================================


- `Fmlib_parse`:

    - module `Position` distinguishes byte and character position. The absolute
      byte offset from the beginning of the file is included.



Release 0.5.5
================================================================================

- `Fmlib_parse`:

    - Fix: In a parser with a separate lexer a syntax error in the lexer
      immedately before the end of input had not been reported correctly.

    - More documentation on parsers with lexers.

    - More functionality in the module `Character` to support writing of lexers.
      E.g. `make_partial`, `restart_partial`, `lexer`.

    - Removed duplicate module `Error_reporter`.


- `Fmlib_browser`:

    - Added subscription `on_animation`.

    - Added `Html.map` and `Attribute.map`.

    - Text in textnodes is now overwritten instead of creating a new node.

- `Fmlib_std`:

    - Improved `Array` and `Btree` to exploit physical identity. An update
      operation where the new element is the same as the original element can be
      ignored.



Release 0.5.4
================================================================================


- `Fmlib_browser`:

    - Added `debug` functions
    - Fix: Set `value` property and not `value` attribute. Reason: The browser
      updates only the property and not the attribute.
    - Added `sandbox_plus` (a sandbox with subscriptions) and
      `basic_application`(an application without javascript communication).
    - Added namespace elements and `svg` elements.
    - Added `Subscription.on_keyup`.
    - Added `Command.map`.
    - Added `Command.just_do` which does not send a message back to the
      application.
    - Added examples from the 7 Guis.



Release 0.5.3
============================================================

- First release of `Fmlib_browser`


Release 0.5.2
============================================================

- Stream module is deprecated in ocaml stdlib. Therefore all usages of the
  Stream module are removed from `Fmlib`.

- `Fmlib_parse`:

    - Some fixes in error reporting
    - Fix: Error in `<?>` operator with layout parsing


Release 0.5.1
============================================================



## `Fmlib_parse`

- Rework of lookahead access. Main function now `fold_lookahead`.

- Added interfaces `MINIMAL_PARSER`, `NORMAL_PARSER`, `FULL_PARSER`, `LEXER`.

- Added `Token_parser` and `Parse_with_lex` to support the separation of parsing
  and lexing.



Release 0.5.0
============================================================

## `Fmlib_std`

- Performance improvement 'Array.replace'

## `Fmlib_pretty`

- Added function 'write_to_channel'

## `Fmlib_parse`

- Changed signature of repetition combinators (e.g. 'one_or_more' and
  'zero_or_more').

- Added a generic parsing combinator for parsing of operator expressions.

- Complete rework of layout parsing.

- Added functions to pretty print error messages and code snippets containing an
  error.

- Documentation for parsing improved and augmented significantly.
