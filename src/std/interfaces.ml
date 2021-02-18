module type ANY =
sig
  type t
end



module type MONAD =
sig
    type _ t

    val return: 'a -> 'a t

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
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
