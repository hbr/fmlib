(** Module to represent the type [int] *)

type t = int

val compare: t -> t -> int
(** [compare i j] compare the two numbers [i] and [j]. *)


val iterate: t -> ('a -> 'a) -> 'a -> 'a
(** [iterate n f start] iterates the function [f] [n] times on the start value
    [start]. *)
