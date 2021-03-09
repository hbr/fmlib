(** A thin wrapper around [Stdlib.Array]

    Use [Stdlib.Array] in case you need functions which are not contained in
    this module. There are no problems using [Std.Array] and [Stdlib.Array],
    because both datatypes are identical.
*)


type 'a t = 'a array

val length: 'a t -> int
(** [length arr] The length of the array [arr]. *)


val get: 'a t -> int -> 'a
(** [get arr i] The [i]th element of the array [arr].

    Precondition: [0 <= i && i < length [arr]]
*)



val map: ('a -> 'b) -> 'a t -> 'b t
(** [map f arr] Create a new array by mapping all elements of the original array
    by the function [f].
*)

val sub: 'a t -> int -> int -> 'a t
(** [sub arr start len] The subarray of [arr] starting at [start] with lenght
    [len].

    Precondition: [0 <= start && start + len <= length arr]
*)


val push: 'a -> 'a t -> 'a t
(** [push a arr] Push element [a] to the rear end of the array [arr]. *)


val to_list: 'a t -> 'a list
(** [to_list arr] Convert the array [arr] to a list with the same content. *)


val of_list: 'a list -> 'a array
(** [of_list lst] Convert the list [lst] to an array with the same content. *)
