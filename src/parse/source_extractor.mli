(** Extract snippets from a source file containing an error. *)

open Fmlib_pretty

type t

(** {1 Make a source extractor from a location in the source file} *)

val of_position: int -> Position.t -> t
(** [of_position extra_lines pos] *)

val of_range: int -> Position.range -> t
(** [of_range extra_lines pos] *)



(** {1 Run the source extractor on a representation of the input} *)

val run_on_string: string -> t -> t
(** Run the extractor on a string representing the input. *)

val run_on_channel: in_channel -> t -> t
(** Run the extractor on a channel representing the input. Note that the channel
    must be positioned at the start. *)



(** {1 Run the source extractor with inverted control} *)

val needs_more: t -> bool

val put: char -> t -> t

val put_end: t -> t


(** {1 Get the document which represents the extracted source snippet} *)
val document: t -> Pretty.t
