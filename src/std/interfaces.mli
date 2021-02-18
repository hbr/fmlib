(** Standard Module Types *)


(** Interface for a module which has a type [t]. *)
module type ANY = sig
  type t
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





(** Interface for a sortable type. A type is sortable, if it has a comparison
    function. *)
module type SORTABLE =
sig
    type t
    (** Type of elements which have a linear order. *)


    val compare: t -> t -> int
    (** [compare a b] Compare the values [a] and [b], both of type [t]. Return
        values:

        - [-1]: [a] strictly smaller than [b]
        - [0]:  [a] and [b] are equal
        - [+1]: [a] is strictly greater than [b]
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
