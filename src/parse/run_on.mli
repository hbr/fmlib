val string_at:
    ('a -> bool) -> (char -> 'a -> 'a) -> ('a -> 'a)
    -> int -> string -> 'a -> int * 'a

val string:
    ('a -> bool) -> (char -> 'a -> 'a) -> ('a -> 'a) -> string -> 'a -> 'a

val channel:
    ('a -> bool) -> (char -> 'a -> 'a) -> ('a -> 'a) -> in_channel -> 'a -> 'a
