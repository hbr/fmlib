(** Standard Module Types *)


(** Interface for a module which has a type [t]. *)
module type ANY = sig
  type t
end





(** Interface for a finite map i.e. a set of key value pairs. *)
module type MAP =
sig
    type key
    (** Type of the keys *)

    type 'a t
    (** Type of a map with keys of type [key] and values of type ['a]. *)


    val is_empty: 'a t -> bool
    (** Is the map empty? *)

    val cardinal: 'a t -> int
    (** [cardinal map] The cardinality of the map i.e. the number of key value
        pairs in the map. *)

    val fold_left:  ('accu -> key -> 'a -> 'accu) -> 'accu -> 'a t -> 'accu
    (** [fold_left f start map]

        Fold the bindings in the map [map] from left to right i.e. lexically
        ascending using the start value [start] for the accumulator and the
        folding function [f] where [f] is applied [f accue key value] yielding a
        new accumulator by consuming one key value pair.
    *)

    val fold_right: ('accu -> key -> 'a -> 'accu) -> 'accu -> 'a t -> 'accu
    (** [fold_left f start map]

        Fold the bindings in the map [map] from right to left i.e. lexically
        descending using the start value [start] for the accumulator and the
        folding function [f] where [f] is applied [f accu key value] yielding a
        new accumulator by consuming one key value pair.
    *)


    val bindings:  'a t -> (key * 'a) list
    (** The list of key value pairs in the map in ascending order. *)


    val find_opt: key -> 'a t -> 'a option
    (** [find_opt key map]

        Find the value which is bound to the key [key] in the map [map]. Return
        [None] if no value is bound to [key].
    *)


    val empty: 'a t
    (** The empty map. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add key value map] Add the key value pair [key, value] to the map. If
        the map has already a key value pair with the key [key] then overwrite
        the old value with the new value.
    *)


    val remove: key -> 'a t -> 'a t
    (** [remove key map] Remove the key value pair with the key [key] from the
        map [map]. If the key is not present, then do nothing. *)


    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
    (** [update key f map]

        Update the value bound to the key [key] in the map [map] by the update
        function [f]. If no value is bound to [key] then [f None] is called. If
        [value] is bound to [key] then [f (Some value)] is called.

        If [f] returns [None] then no value is added and the old binding is
        deleted (if it existed before).

        If [f] return [Some new_value] then the old value is updated, if
        existed, or the new value is added if no old value existed before.
    *)
end




(** Interface for a module with a monadic container. *)
module type MONAD =
sig
    type _ t
    (** ['a t] is a monadic container with elements of type ['a]. *)

    val return: 'a -> 'a t
    (** [return a] puts the elements [a] into a monadic container. *)

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    (** [m >> f] extracts elements of of the monadic container [m] and applies
        the function [f] to them which puts them back into a monadic container.
    *)

    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    (** [let* a = m in f a] is the same as [m >>= f]. *)
end


(** Interface for a finite set. *)
module type SET =
sig
    type item
    (** Type of elements of the set. *)

    type t
    (** Type of the set of elements of type [item]. *)

    val is_empty: t -> bool
    (** Is the set empty? *)


    val cardinal: t -> int
    (** The cardinality of the set i.e. the number of its elements. *)


    val mem: item -> t -> bool
    (** [mem element set] Is [element] a member of the set [set]? *)


    val fold_left:  ('accu -> item -> 'accu) -> 'accu -> t -> 'accu
    (** [fold_left f start set]

        Fold the elements of the set [set] from left to right i.e. lexically
        ascending using the start value [start] for the accumulator and the
        folding function [f] where [f] is applied [f accu element] yielding a
        new accumulator value by consuming one element.
    *)

    val fold_right: ('accu -> item -> 'accu) -> 'accu -> t -> 'accu
    (** [fold_left f start set]

        Fold the elements of the set [set] from right to left i.e. lexically
        descending using the start value [start] for the accumulator and the
        folding function [f] where [f] is applied [f accu element] yielding a
        new accumulator value by consuming one element.
    *)

    val elements: t -> item list
    (** The elements of the set in ascending order returned as a list. *)

    val empty: t
    (** The empty set. *)

    val add:    item -> t -> t
    (** [add element set] Add the element [element] to the set [set]. If the
        element is already in the set, then do nothing. *)

    val remove: item -> t -> t
    (** [remove element set] Remove the element [element] from the set [set]. If
        the element is not in the set, then do nothing. *)
end





(** Interface for a sortable type. A type is sortable, if it has a comparison
    function. *)
module type SORTABLE =
sig
    type t
    (** Type of elements which have a linear order. *)


    val compare: t -> t -> int
    (** [compare a b] Compare the values [a] and [b], both of type [t]. Return
        values:

        [compare a b < 0]               if and only if [a < b]

        [compare a b = 0]               if and only if [a = b]

        [compare a b > 0]               if and only if [a > b]
    *)
end




(** Interface for a source of items i.e. a stream. *)
module type SOURCE =
sig
    type item
    (** Type of items coming from the source. *)


    type t
    (** [t] A stream of items. *)


    val has_more: t -> bool
    (** [has_more s] Does the stream [s] have more items? *)


    val peek: t -> item
    (** [peek s] Peek the next item from the stream [s].

        Precondition: [has_more s]
    *)

    val advance: t -> t
    (** [advance s] Pop the top item off the stream [s].

        Precondition: [has_more s]
    *)
end



(** Interface for a sink of items. *)
module type SINK =
sig
    type item
    (** Type of items to be received by the sink. *)


    type t
    (** [t] A sink for items. *)


    val needs_more: t -> bool
    (** [needs_more s] Is the sink [s] able to accept more items? *)


    val put: item -> t -> t
    (** [put item sink] Put the item to the sink.

        Precondition: [needs_more sink].
    *)


    val put_end: t -> t
    (** [put_end sink] signals to the sink that there are no more items coming.
     *)
end
