open Fmlib_std

module Text:
sig
    type t

    val substring: string -> int -> int -> t
    val string: string -> t
    val fill: int -> char -> t
    val char: char -> t
    val length: t -> int
    val peek: t -> char
    val advance: t -> t option
end


module Line:
sig
    type t
end


type chunk
type group


module Chunk:
sig
    type t = chunk
    val break_text: t -> string
    val line: t -> Line.t
    val texts: t -> Text.t Deque.t
    val groups: t -> group Deque.t
end



module Group:
sig
    type t = group
    val length: t -> int
    val complete_groups: t -> t Deque.t
    val chunks: t -> Chunk.t Deque.t
end




module Buffer:
sig
    type t
    val count: t -> int
    val length: t -> int
    val pop: t -> (Group.t * t) option
end




type t

val init: int -> int -> t

val line_indent: t -> int

val advance_position: int -> t -> t

val newline: t -> t
val newline_with_line: Line.t -> t -> t

val fits: int -> t -> bool
val buffer_fits: t -> bool

val is_buffering: t -> bool
val direct_out:   t -> bool
val line_direct_out: t -> bool
val within_active:   t -> bool

val push_text: Text.t -> t -> t
val push_break: string -> t -> t

val pull_buffer: t -> Buffer.t * t
val flatten_done: t -> t
val effective_done: Buffer.t -> int -> t -> t

val enter_group: t -> t
val leave_group: t -> t

val increment_indent: int -> t -> t
val width: int -> t -> int * t
val ribbon: int -> t -> int * t
