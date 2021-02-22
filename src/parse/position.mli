(** Represent a position in a text file. *)


type t (** Position in a text file. *)


type range = t * t (* A range in a text file. *)





val start: t
(** Position with points to the start of a textfile. *)



val line: t -> int
(** [line pos] The line number corresponding to the position [p]. Note: The
    first line is line 0. *)



val column: t -> int
(** [column pos] The column number corresponding to the position [p]. Note: The
    first column is column 0. *)



val next: char -> t -> t
(** [next next_char pos]: Advance the position by using the next character. If
    the next character is a newline, then the line number is increment and the
    column number is reset to 0.
*)



val next_line: t -> t
(** [next_line pos] Advance the position to the start of the next line. *)



val next_column: t -> t
(** [next_column pos] Advance the column position by 1. *)
