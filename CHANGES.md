# Release 0.3.0

Additions to `Fmlib_std`

- Module `Array`:

    - Added binary search algorithm
    - Added functions `insert`, `replace`, `remove`, ...

- New module `Btree` with an implementation for finite sets and finite maps.


Separated the libraries `Fmlib_std`, `Fmlib_parse` and `Fmlib_pretty` into
separate opam packages.

Added a design document in the form of a sphinx document which is published to
`readthedocs`.



# Release 0.2.0



## New Library `Fmlib_js`

Primitives to interface to javascript via `js_of_ocaml`.


## `Fmlib_parse`

- Parsing of base64 encoded strings

- Parser combinators `range` and `hex_digit`


## `Fmlib_std`

- Added module `Option`

- Added module `Void` for not inhabited types

- Added `Array.of_list`





# 0.1.0 Initial release

First release of `fmlib` containing the toplevel modules

- `Fmlib_std`
- `Fmlib_pretty`
- `Fmlib_parse`
