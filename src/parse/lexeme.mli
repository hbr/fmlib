(** Helper module to make parsing with {!module:Character} more convenient. *)

(** The module needs a subset of the combinators of the module
    {!module:Character} and some definitions of whitespace and identifiers in
    order to provide more convenient combinators than the module
    {!module:Character} has.
*)

open Lexeme_intf

module Make: MAKE
