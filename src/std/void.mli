(** [Void.t] is the type of an object which cannot exist. *)

type t
(** Type of a nonexisting object. *)


val absurd: t -> 'a
(** [absurd n] Return an object of any type.

    This function is possible, because there never can be an object [n] of type
    [t].
*)
