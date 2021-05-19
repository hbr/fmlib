# Fmlib - Functional Library with Managed Effects for Ocaml


## Overview

`Fmlib` is a functional library which has components for

- Standard Data Structures (B trees, ... ): `Fmlib_std`

- Pretty Printing: `Fmlib_pretty`

- Parsing: `Fmlib_parse`

- Access to the browser and nodejs: `Fmlib_js`.


[API Documentation](https://hbr.github.io/fmlib/odoc/fmlib)


- [`Fmlib_std`](https://hbr.github.io/fmlib/odoc/fmlib_std)

- [`Fmlib_pretty`](https://hbr.github.io/fmlib/odoc/fmlib_pretty)

- [`Fmlib_parse`](https://hbr.github.io/fmlib/odoc/fmlib_parse)

- [`Fmlib_js`](https://hbr.github.io/fmlib/odoc/fmlib_js)



[Design Documentation](https://fmlib_ocaml.readthedocs.io): Some designs and the
corresponding algorithms are documented separately to ensure correctness of the
corresponding designs and algorithms. The design documention is not neccessary
to understand the usage of the library from a user's perspective. The user's
perspective is documented in the API.






# Installation

It is best to install the libraries via the ocaml package manager `opam`.

    opam install fmlib_std
    opam install fmlib_pretty
    opam install fmlib_parse
    opam install fmlib_js


# Usage with `dune`

A program `foo` which uses e.g. the library `fmlib_std` can be e.g. compiled by
dune with the dune file

    (executable
        (name foo)
        (libraries
            fmlib_std
            ...
        )
    )
