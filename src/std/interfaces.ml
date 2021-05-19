module type ANY =
sig
  type t
end


module type MAP =
sig
    type key
    type 'a t
    val is_empty: 'a t -> bool
    val cardinal: 'a t -> int
    val fold_left:  ('accu -> key -> 'a -> 'accu) -> 'accu -> 'a t -> 'accu
    val fold_right: ('accu -> key -> 'a -> 'accu) -> 'accu -> 'a t -> 'accu
    val bindings:  'a t -> (key * 'a) list

    val find_opt: key -> 'a t -> 'a option

    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val remove: key -> 'a t -> 'a t
    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
end



module type MONAD =
sig
    type _ t

    val return: 'a -> 'a t

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
end


module type SET =
sig
    type item
    type t
    val is_empty: t -> bool
    val cardinal: t -> int
    val mem: item -> t -> bool
    val fold_left:  ('accu -> item -> 'accu) -> 'accu -> t -> 'accu
    val fold_right: ('accu -> item -> 'accu) -> 'accu -> t -> 'accu
    val elements: t -> item list
    val empty: t
    val add:    item -> t -> t
    val remove: item -> t -> t
end


module type SORTABLE =
sig
    type t
    val compare: t -> t -> int
end


module type SOURCE =
sig
    type item
    type t
    val has_more: t -> bool
    val peek: t -> item
    val advance: t -> t
end




module type SINK =
sig
    type item
    type t
    val needs_more: t -> bool
    val put: item -> t -> t
    val put_end: t -> t
end
