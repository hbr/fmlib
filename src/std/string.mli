(** A small wrapper around [Stdlib.String] with some extra functions.

    Use [Stdlib.String] if you need functions from the ocaml standard library
    which are not in this module.
*)




type t = string
(** A standard ocaml string. *)


val is_prefix: t -> t -> bool
(** [is_prefix a b] Is [a] a prefix of [b]? *)

val is_suffix: t -> t -> bool
(** [is_suffix a b] Is [a] a suffix of [b]? *)


val reverse: t -> t
(** [reverse s] reverses the string [s]. *)


val compare: t -> t -> int
(** [compare s1 s2] Compare the strings [s1] and [s2].

    Return [-1], if [s1] is lexicographically smaller than [s2]
    Return [0],  if both string are equal
    Return [+1], if [s1] is lexicographically greater than [s2]
*)


val one: char -> t
(** [one c] A string with the character [c] as the only character. *)



val find: (char -> bool) -> int -> t -> int
(** [find p start str] Find the position of the first character starting from
    [start] in the string [str] which satisfies the predicate [p]. If no
    character can be found return the length of the string.
*)


val has:  (char -> bool) -> int -> t -> bool
(** [has p start str] Does the string [str] starting from position [start] have
    a character satisfying the predicate [p]? *)


val find_bwd: (char -> bool) -> int -> t -> int
(** [find_bwd p beyond str] Find the position of the first character before
    [beyond] in the string [str] which satisfies the predicate [p]. Return [-1],
    if no character can be found. *)


val list: t -> char list
(** [list str] Convert the string [str] to a list of characters. *)

val of_list: char list -> t
(** [of_list l] Convert the list [l] of characters to a string. *)


val length: t -> int
(** [length str] The length of the string [str]. *)


val get: t -> int -> char
(** [get str i] The [i]th character of the string [str].

    Precondition: [0 <= i && i < length str]
*)


val sub: t -> int -> int -> t
(** [sub str start len] The substring of [str] starting at [start] with length
    [len].

    Precondition: [0 <= start <= start + len <= length str]
*)


val concat: string -> string list -> string
(** [concat sep str_list] Concatenate the strings in the string list [str_list]
    and put the separator [sep] between them.
*)


val split_on_char: char -> string -> string list
(** [split_on_char c str] Split the string [str] on each occurrence of the
    character [c] into a list of strings.
*)


val make: int -> char -> t
(** [make n c] Make a string with [n] copies of the character [c]. *)


val init: int -> (int -> char) -> t
(** [init n f] Make a string of length [n] where the [i]th character is [f i].
*)





(** Conversion of a string to a source of characters. *)
module To_source:
sig
    include Interfaces.SOURCE with type item = char

    val make: string -> t
    (** [make str] A character stream generated from the string [str]. *)
end



(** Conversion of a source of characters to a string. *)
module From_source (S: Interfaces.SOURCE with type item := char):
sig
    val make: S.t -> t
    (** [make s] Convert the stream [s] of characters to a string. *)


    val make_with_size: int -> S.t -> t
    (** [make_with_size n s] Convert the stream [s] of characters to a string
        where [n] is an estimate of the length of the string.

        The function works internally with a buffer. [n] is used to guide the
        buffer allocation and the resizing of the buffer. The better the
        estimate, the fewer buffer allocations and resizes are necessary.
    *)
end
