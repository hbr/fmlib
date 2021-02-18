(** A thin wrapper around [Stdlib.List] which avoids throwing exceptions and
    with some additional monadic functions.

    In case you need functions from the module [List] of the ocaml standard
    library, just use [Stdlib.List]
*)


open Interfaces


(** {1 List Monad}*)

type 'a t = 'a list
(** A list of values of type ['a]. *)


val return: 'a -> 'a t
(** [return a] makes a singleton list with the element [a]. *)


val (>>=): 'a t -> ('a -> 'b t) -> 'b t
(** [l >>= f] applies the function [f] to all elements of the list [l] and
   concatenates all lists. *)


val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
(** [let* a = m in f a] is equivalent to [m >>= f]. *)


val (>=>): ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
(** [f >=> g] composes the two monadic functions [f] and [g]. *)


val (<*>): ('a -> 'b) t -> 'a t -> 'b t
(** [flst <*> lst] is equivalent to [flst >>= fun f -> map f lst] i.e. it maps
   all functions contained in [flst] over the list [lst] and then concatenates
   the results. *)



val join: 'a list list -> 'a list
(** [join] is the same as {!val:concat}. *)





(** {1 Modified list functions}*)

val find: ('a -> bool) ->'a t -> 'a option
(** [find p l] finds an element [e] in the list [l] which satisfies [p e]. *)





(** {1 List functions from Stdlib}*)

val append: 'a list -> 'a list -> 'a list
(** [append a b] concatenate the lists [a] and [b].

    Synonym [a @ b].
*)



val concat: 'a list list -> 'a list
(** [concat ll] concatenates all lists contained in the list of lists [ll]. *)


val split: ('a * 'b) list -> 'a list * 'b list
(** Transform a list of pairs into a pair of lists. *)


val rev: 'a list -> 'a list
(** [rev a] reverses the list [a]. *)


val rev_append: 'a list -> 'a list -> 'a list
(** [rev_append a b] prepends the lists [rev a] in front of the list [b]. *)



val length: 'a t -> int
(** [length l] The length of the list [l]. *)


val filter: ('a -> bool) -> 'a t -> 'a t
(** [filter p l] Returns a list with all the elements of [l] which satisfy the
    predicate [p]. *)


val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
(** [fold_left f s l]

    Compute [f (f ... (f a b1) ... bn-1) bn] where [l = [b1; ...; bn-1; bn]]
*)


val fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
(** like [fold_left], just iterate from right to left. *)


val map : ('a -> 'b) -> 'a list -> 'b list
(** [map f l] returns a list where all elements of [l] are mapped by the
    function [f]. *)


val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** [mapi f l] map all elements of the list [l] with the mapping function [f]
    which receives the index of the element (starting at zero) and the element.
*)


val rev_map : ('a -> 'b) -> 'a list -> 'b list
(** [rev_map f l] The same as [map (rev l)]. [rev_map] is tail recursive. *)


val for_all : ('a -> bool) -> 'a list -> bool
(** [for_all p l] checks, if all elements in the list [l] satisfy the predicate
    [p].
*)


val exists : ('a -> bool) -> 'a list -> bool
(** [exists p l] checks, if some element in the list [l] satisfies the predicate
    [p].
*)





(** {1 Additional list functions}*)


val split_head_tail: 'a t -> 'a * 'a t
(** [split_head_tail l] split the list in its head and tail part.

    Precondition: The list is not empty.
*)



val map_and_filter: ('a -> 'b option) -> 'a list -> 'b list
(** [map_and_filter f list] maps the list with [f] and removes the element for
    which [f e = None].*)


val split_at: ('a -> bool) -> 'a t -> 'a t * 'a t
(** [split_at p lst] scans the list until it finds the first element
    satisfying [p] and returns the prefix and the remainder starting at the
    encountered element. If the second list is empty, then there is no element
    in the list satisfying [p]. *)



val transpose: 'a list list -> 'a list list
(**
    [transpose list_of_rows] returns the list of columns.

    Preconditions:

    - The list of rows must not be empty.

    - All rows in the list of rows must not be empty and have the same length.

    Example:
    {[
        transpose [ [1; 2; 3]; [4; 5; 6] ]
        =
        [ [1; 4]; [2; 5]; [3; 6] ]
    ]}
*)





(** {1 Monadic list functions}*)

(** Monadic list functions *)
module Monadic (M: MONAD):
sig
  (** [fold_left f lst start] leftfolds the function [f] over the list [lst]
     starting with the value [start].  Continuation of the fold is determined
     by the bind operator [>>=] of the monad [M]. E.g. if the monad [M] is
     [Option] the folding stops as soon as [f e acc] returns the value [None].
   {[
        fold_left f [a b c ...] s =
          M.(f a s   >>= fun acc ->
             f b acc >>= fun acc ->
             f c acc >>= fun acc ->
             ...)
     ]}
 *)
  val fold_left:  ('a -> 'b -> 'b M.t) -> 'a t -> 'b -> 'b M.t

  (** The same as [fold_left] just right folding.
   {[
        fold_right f [... x y z] s =
        fold_left f (rev [... x y z]) s =
          M.(f z s   >>= fun acc ->
             f y acc >>= fun acc ->
             f x acc >>= fun acc ->
             ...)
   ]}*)
  val fold_right: ('a -> 'b -> 'b M.t) -> 'a t -> 'b -> 'b M.t



  (** The same as [fold_left] except that the folding function receives the
     position of the first argument in the list as an additional argument. *)
  val foldi_left: (int -> 'a -> 'b -> 'b M.t) -> 'a t -> 'b -> 'b M.t
end
