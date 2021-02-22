module type PARSER =
sig
end




module type COMBINATOR =
sig
    type state
    type expect
    type semantic

    type _ t
    val (>>=):   'a t -> ('a -> 'b t) -> 'b t
    val (let* ): 'a t -> ('a -> 'b t) -> 'b t
    val map: ('a -> 'b) -> 'a t -> 'b t
    val succeed: 'a -> 'a t
    val return:  'a -> 'a t
    val unexpected: expect -> 'a t
    val clear_last_expectation: 'a -> 'a t
    val fail: semantic -> 'a t
    val (</>):   'a t -> 'a t -> 'a t
    val choices: 'a t -> 'a t list -> 'a t
    val (<?>):   'a t -> expect -> 'a t



    val get: state t
    val update: (state -> state) -> unit t
    val get_and_update: (state -> state) -> state t



    val optional: 'a t -> 'a option t
    val zero_or_more: 'r -> ('item -> 'r -> 'r) -> 'item t -> 'r t

    val one_or_more:
        ('item -> 'r)
        -> ('item -> 'r -> 'r)
        -> 'item t
        -> 'r t

    val list_zero_or_more: 'a t -> 'a list t

    val list_one_or_more:  'a t -> ('a * 'a list) t

    val skip_zero_or_more: 'a t -> int t


    val skip_one_or_more:  'a t -> int t


    val one_or_more_separated:
        ('item -> 'r)
        -> ('r -> 'sep -> 'item -> 'r)
        -> 'item t
        -> 'sep t
        -> 'r t

    val backtrack: 'a t -> expect -> 'a t

    val followed_by: 'a t -> expect -> 'a t

    val not_followed_by: 'a t -> expect -> unit t
end
