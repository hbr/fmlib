(** Represent a position in a text file. *)


(**

    A position in a text file is represented by a line number and a column
    number, both starting with zero. So the first line is line number zero and
    the first column is column number zero.

    Sometimes the absolute byte offset since the beginning of the file is
    important. Therefore the absolute byte offset for the last newline is
    included as well.

    Furthermore it is important to distinguish between the byte position and the
    character position within a line. Therefore a correction offset is needed
    which is added to the byte column to get the character column.

    An object of type [Position.t] has the following content:

    - Byte offset from the beginning of the stream to the beginning of the
    previous line.

    - Number of the current line (zero based)

    - Byte position within the current line (zero based)

    - Correction between the byte position and the visible character position on
   the current line.

    By default all bytes between space and ascii 126 and between decimal 128 and
    decimal 255 are treated as one character. The tab character is treated as 4
   characters and the newline character increments the line number and resets
   the column number. All other bytes between decimal 0 and before the space are
   treated as zero characters.

    This default handling is done by the function {!next}. For more special
    treatments (e.g. unicode characters) there is the function {!correct} to
    adapt the character position with respect to the by position and the
    functions {!advance} and {!newline} for find granular control of the
    position.
*)





(** {1 Basics} *)



type t (** Position in a text file. *)






val start: t
(** Position with points to the start of a textfile (line = 0, column = 0). *)







(** {1 Line and column number} *)


val line: t -> int
(** [line pos] The line number corresponding to the position [p]. Note: The
    first line is line 0. *)


val column: t -> int
(** [column pos] The character position within the current line of [pos].

    For differences between [column] and [byte_column] see {!byte_column}.

    Note: The
    first column is column 0.
*)





(** {1 Byte offsets} *)




val byte_offset_bol: t -> int
(** [byte_offset_bol pos] The byte offset since the beginning of the file at the
    last newline. *)


val byte_column: t -> int
(** [byte_column pos] The byte column number corresponding to the position [p].

    Note: If the line up to the position has only printable ascii or latin-1
    characters, then the [byte_column p] is the same as [column p]. If the line
    has some multibyte unicode characters, then both columns might be different.
    Notprintable ascii characters i.e. characters before the space character
    increase the byte column, but not the column.
*)



val byte_offset: t -> int
(** [byte_offset pos] The byte offset of the current position since the
    beginning of the file.
 *)






(** {1 Increment position} *)


val next: char -> t -> t
(** [next next_char pos]: Advance the position by using the next character. If
    the next character is a newline, then the line number is increment and the
    column number is reset to 0.
*)



val correct: int -> t -> t
(** [correct n pos] Correct the column by [n]. In case of multibyte characters
    like unicode characters the correction must be negative.
 *)


val advance: int -> int -> t -> t
(** [advance byte_width width pos] Advance the position by [byte_width] and
    character width [width].
*)



val newline: int -> t -> t
(** [newline byte_width pos] Start a new line i.e. set the character position
    and byte position to zero and increment the line number.
*)







(** {1 Compare positions} *)

val is_less_equal: t -> t -> bool
(** [is_less_equal p1 p2] Are [p1] and [p2] in the correct order i.e. [p2] is
    not before [p1]. *)









(** {1 Ranges (i.e. pairs of positions)} *)


type range = t * t (* A range in a text file. *)



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
