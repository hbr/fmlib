(** A thin wrapper around [Stdlib.Array] with additional functions and sets and
    maps based on arrays

    Use [Stdlib.Array] in case you need functions which are not contained in
    this module. There are no problems using [Std.Array] and [Stdlib.Array],
    because both datatypes are identical.

    Futhermore there are the modules [Set] and [Map] which implement finite sets
    and finite maps based on sorted arrays. For small sets and maps, the array
    based implementations are superior to tree based implementations like avl
    trees or red black trees, because they have a better cache behaviour.
*)



(** {1 Basic Array Functions} *)

type 'a t = 'a array

val length: 'a t -> int
(** [length arr] The length of the array [arr]. *)


val valid_index: int -> 'a t -> bool
(** [valid_index i arr] Is [i] a valid index into the array [arr]? *)


val is_empty: 'a t -> bool
(** Is the array empty? *)


val has_some: 'a t -> bool
(** Does the array have at least one element? *)


val get: 'a t -> int -> 'a
(** [get arr i] The [i]th element of the array [arr].

    Precondition: [0 <= i && i < length [arr]]
*)


val first: 'a t -> 'a
(** [first xs] The first element of the array [xs].

    Precondition: [has_some xs]
*)


val last: 'a t -> 'a
(** [last xs] The last element of the array [xs].

    Precondition: [has_some xs]
*)




val set: 'a t -> int -> 'a -> unit
(** [set arr i value] Set the [i]th element of the array [arr] to [value].

    Precondition: [0 <= i && i < length [arr]]
*)


val make: int -> 'a -> 'a t
(** Same as Stdlib.Array.make *)


val init: int -> (int -> 'a) -> 'a t
(** Same as Stdlib.Array.init *)



val append: 'a t -> 'a t -> 'a t
(** [append xs ys] Concatenate the two arrays [xs] and [ys]. *)



val insert: int -> 'a -> 'a t -> 'a t
(** [insert i x xs] Insert the element [x] at position [i] into the array [xs].

    Make place by pushing up the elements [i], [i + 1], ... one position.

    Precondition: [0 <= i && i <= length xs].
*)



val replace: int -> 'a -> 'a t -> 'a t
(** [replace i x xs] Replace the [i]th element of [xs] by [x].

    Precondition. [0 <= i && i < length xs]
*)



val remove: int -> 'a t -> 'a t
(** [remove i xs] Remove the [i]th element from the array [xs].

    Precondition: [0 <= i && i < length xs].
*)



val remove_first: 'a t -> 'a t
(** [remove_first xs] Remove the first element from the array [xs].

    Precondition: [has_some xs]
*)



val remove_last: 'a t -> 'a t
(** [remove_last xs] Remove the last element from the array [xs].

    Precondition: [has_some xs]
*)



val map: ('a -> 'b) -> 'a t -> 'b t
(** [map f arr] Create a new array by mapping all elements of the original array
    by the function [f].
*)



val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f start arr]

    Fold the folding function [f] with start value [start] over the array [arr].

    Compute
    {[
        (f (... (f (f start arr.(0)) arr.(1)) ...) arr.(n - 1)
    ]}

    where [n = length arr].
*)


val foldi_left: ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [foldi_left f start arr]

    Like [fold_left] with the current index as an additional argument to the
    folding function.
*)


val fold_right: ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
(** [fold_right f arr start]

    Compute
    {[
        f arr.(0) (f arr.(1) ( ... (f arr.(n - 1) start) ... ))
    ]}
    where [n = length arr]
*)


val copy: 'a t -> 'a t
(** Make a copy of the array. *)



val sub: 'a t -> int -> int -> 'a t
(** [sub arr start len] The subarray of [arr] starting at [start] with lenght
    [len].

    Precondition: [0 <= start && start + len <= length arr]
*)



val blit: 'a t -> int -> 'a t -> int -> int -> unit
(** [blit src src_pos dst dst_pos len]

    Copy [len] values from array [src] starting at [src_pos] to array [dst]
    starting at [dst_pos].
*)


val find: ('a -> bool) -> 'a t -> int option
(** [find p arr]

    Find the element satisfying the predicate [p] in the array [arr]. Return
    [None] if no such element exists.
*)


val for_all: ('a -> bool) -> 'a t -> bool
(** [for_all p arr]

    Do all elements of the array [arr] satisfy the predicate [p]?
*)



val exists: ('a -> bool) -> 'a t -> bool
(** [exists p arr]

    Exists an element of the array [arr] which satisfies the predicate [p]?
*)




val push: 'a -> 'a t -> 'a t
(** [push a arr] Push element [a] to the rear end of the array [arr]. *)


val push_front: 'a -> 'a t -> 'a t
(** [push_front a arr] Push element [a] to the front end of the array [arr]. *)


val to_list: 'a t -> 'a list
(** [to_list arr] Convert the array [arr] to a list with the same content. *)


val of_list: 'a list -> 'a array
(** [of_list lst] Convert the list [lst] to an array with the same content. *)





(** {1 Binary Search} *)


val binsearch: ('key -> 'key -> int) -> ('a -> 'key) -> 'key -> 'a t -> int * bool
(** [binsearch compare key_of key arr]

    Search the position of [key] in [arr]. Assume that the array [arr] is sorted
    without duplicates. It returns the pair [position, exact_flag] with the meaning

    {[
        exact_flag          =>      key = key_of arr.(position)

        not exact_flag      =>      key < key_of arr.(position)
    ]}

    Corner case: [position = length arr, exact_flg = false]. This corresponds to
    a fictitious key of [+infinity] at the illegal position [length arr].

    The array [arr] consists of elements of type ['a]. The function [key_of]
    extracts a key from an element of the array. The keys are compared using the
    comparison function [compare] with the usual meaning:

    [compare a b < 0]               if and only if [a < b]

    [compare a b = 0]               if and only if [a = b]

    [compare a b > 0]               if and only if [a > b]
*)







(** {1 Sets and Maps based on arrays} *)



(** A set based on arrays *)
module Set (Key: Interfaces.SORTABLE):
sig
    include Interfaces.SET with type item = Key.t

    val element: int -> t -> Key.t
    (** [element i set] The [i]th element of the set. *)


    val index_of: Key.t -> t -> int option
    (** [index_of e set] Compute the index of the element [e] within the set
        [set], if [e] is in the set. *)


    val singleton: Key.t -> t
    (** [singleton e] The singleton set with the only element [e]. *)
end






(** A map based on arrays *)
module Map (Key: Interfaces.SORTABLE):
sig
    include Interfaces.MAP with type key = Key.t


    (** [pair i map] The [i]th key value pair of [map].

        Precondition: [0 <= i && i < cardinal map]
    *)
    val pair: int -> 'a t -> Key.t * 'a


    (** [singleton key value] The map with the only key value pair
        [(key,value)]. *)
    val singleton: Key.t -> 'a -> 'a t
end
