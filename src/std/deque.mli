(** A double ended queue. Insert from both ends, pop only from the front. *)


type _ t
(** ['a t] A double ended queue with element type ['a]. *)

val is_empty: _ t -> bool
(** [is_empty q] Is the queue [q] empty? *)


val has_some: _ t -> bool
(** [has_some q] Does the queue have elements? *)


val empty: _ t
(** [empty] The empty queue. *)


val push_front: 'a -> 'a t -> 'a t
(** [push_front e q] Push the element [e] to the front of the queue [q]. *)


val push_rear: 'a -> 'a t -> 'a t
(** [push_rear e q] Push the element [e] to the rear of the queue [q]. *)


val pop_front: 'a t -> ('a * 'a t) option
(** [pop_front q] Pop the front element. *)


val update_first: ('a -> 'a) -> 'a t -> 'a t
(** [update_first f q] Update the first element of the queue [q] with the update
    function [f]. *)


val update_last: ('a -> 'a) -> 'a t -> 'a t
(** [update_last f q] Update the last element of the queue [q] with the update
    function [f]. *)


val to_list: 'a t -> 'a list
(** [to_list q] Convert the queue [q] to a list. *)
