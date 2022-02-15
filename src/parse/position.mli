(** Represent a position in a text file. *)


type t (** Position in a text file. *)


type range = t * t (* A range in a text file. *)





val start: t
(** Position with points to the start of a textfile (line = 0, column = 0). *)



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



val is_less_equal: t -> t -> bool
(** [is_less_equal p1 p2] Are [p1] and [p2] in the correct order i.e. [p2] is
    not before [p1]. *)



val is_valid_range: range -> bool
(** Is the range a valid range in a file i.e. are both positions valid and is
    the first position before or at the second position?
 *)



val merge: range -> range -> range
(** [merge range_1 range_2]

    Merge the ranges [range_1] and [range_2] i.e. when [range_1 = (pos1, _)] and
    [range_2 = (_, pos2)] then the merged range is [(pos1, pos2)].

    Precondition: Both are valid ranges and [range_1] starts before [range_2]
    and [range_1] ends before [range_2].
*)
