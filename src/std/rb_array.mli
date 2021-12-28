(** A Radix Balanced Array. A functional long array which allows fast random
    access, fast insertion and deletion at the rear end.
 *)


(**
    In functional programming lists are one of the most pervasive
    datastructures. They allow fast insertion and deletion at one end (in case of
    lists at the front end). But they have one disadvantage: Randow access is
    very expensive. In order to retrieve the [i]th element a procedure of time
    complexity [i] is needed.

    Arrays allow fast random access. However insertion and deletion is
    expensive, because copying and shifting of the elements is required to
    insert or delete elements. The performance hit can be significant for long
    arrays.

    However insertion and deletion in arrays is cheap as long the array fits
    into the cache line of a modern microprocessor.

    The radix balanced array stores the information within array chunks which
    are not bigger than a cache line (usually 32 machine words or more on todays
    microprocessors).

    In order to have a fast random access a radix balanced tree structure is
    used where each interior node fits into a cache line as well.

    E.g. in radix balanced arrays it is possible to access individual elements
    of the array with 3 indirections if the array has one million elements.

*)

type 'a t
(** A radix balanced array storing elements of type ['a]. *)


val length: 'a t -> int
(** The number of elements in the array. *)


val is_empty: 'a t -> bool
(** Is the array empty? *)


val has_some: 'a t -> bool
(** Is there at least one element in the array? *)


val first: 'a t -> 'a
(** [first t] The first element of the array [t].

    Precondition: [has_some t]
*)

val last: 'a t -> 'a
(** [last t] The last element of the array [t].

    Precondition: [has_some t]
*)


val element: int -> 'a t -> 'a
(** [element i t] The [i]th element of the array [t].

    Precondition: [i < length t]
*)


val empty: 'a t
(** The empty array. *)


val replace: int -> 'a -> 'a t -> 'a t
(** [replace i e t] Replace the [i]th element of the array [t] by the new
    element [e].

    Precondition: [i < length t]
*)


val push: 'a -> 'a t -> 'a t
(** [push e t] Append the element [e] at the rear end of the array [t].
 *)


val pop: 'a t -> 'a * 'a t
(** [pop t] Pop the last element off the array [t] and return the pair
    consisting of the last element and the array where the last element has been
    removed.

    Precondition: [has_some t]
*)


val pop_opt: 'a t -> ('a * 'a t) option
(** [pop t] Pop the last element off the array [t] and return the pair
    consisting of the last element and the array where the last element has been
    removed. Return [None] in case the array [t] is empty.
*)


val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f start t] Fold the array [t] from left to right using the folding
    function [f] with start value [start].

    Same as [fold_left] on lists and plain arrays.
*)



val foldi_left: ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Like [fold_left] with the current element index as an additional argument.
 *)
