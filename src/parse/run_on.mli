val string_at:
    ('a -> bool) -> (char -> 'a -> 'a) -> ('a -> 'a)
    -> int -> string -> 'a -> int * 'a

val string:
    ('a -> bool) -> (char -> 'a -> 'a) -> ('a -> 'a) -> string -> 'a -> 'a

val channel:
    ('a -> bool) -> (char -> 'a -> 'a) -> ('a -> 'a) -> in_channel -> 'a -> 'a






module Make (CD: Interfaces.CHAR_DECODER):
sig
    val string_at:
        ('a -> bool) -> (CD.t -> 'a -> 'a) -> ('a -> 'a)
        -> int -> string -> 'a -> int * 'a

    val string:
        ('a -> bool) -> (CD.t -> 'a -> 'a) -> ('a -> 'a) -> string -> 'a -> 'a

    val channel:
        ('a -> bool) -> (CD.t -> 'a -> 'a) -> ('a -> 'a) -> in_channel -> 'a
        -> 'a
end
