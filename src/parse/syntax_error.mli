(** Generate a pretty printing document from a syntax error. *)

open Fmlib_pretty.Print


val document: int -> (string * Indent.expectation option) list -> doc
