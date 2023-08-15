open Fmlib_std

module type DICT =
sig
    type key

    type _ t

    val empty: 'a t

    val map: ('a -> 'b) -> 'a t -> 'b t

    val find_opt: key -> 'a t -> 'a option

    val add: key -> 'a -> 'a t -> 'a t

    val set: key -> ('a option -> 'a) -> 'a t -> 'a t

    val of_list: (key * 'a) list -> 'a t

    val fold: ('accu -> key -> 'a -> 'accu) -> 'accu -> 'a t -> 'accu

    val iter: (key -> 'a -> unit) -> 'a t -> unit

    val diff:
        (key -> 'a -> unit)
        -> (key -> 'a -> unit)
        -> (key -> unit)
        -> 'a t
        -> 'a t
        -> unit
end

module Make (Key: Interfaces.SORTABLE):
sig
    include DICT with type key := Key.t
end


include DICT with type key := string
