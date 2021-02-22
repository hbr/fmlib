(** The allowed indentations: Helper module for indentation sensitive parsing.

*)


type violation =
    | Indent of int
    | Align of int
    | Align_between of int * int


type t
(** Allowed indentations *)


val initial: t
(** Initially all indentations 0,1,... are allowed and no alignment is required.

*)


val check_position: int -> t -> violation option
(** [check_position col ind] Return a violation, if [pos] is not an allowed
    indentation position. Otherwise return [None]. *)


val token: int -> t -> t
(** [token pos ind] Accept a token at column [pos].

    Preconditions: [is_position_allowed pos ind].
*)


val align: t -> t
(** [align ind] Set the alignment flag.

    The next token sets the indentation set to [{pos}] where [pos] is the column
    of the token and clears the aligment flag.
*)


val left_align: t -> t
(** [left_align ind] Set the alignment flag and the indentation set to
    [{pos}] where [pos] is the lower bound of the current set of indentation
    positions. *)


val start_indent: int -> t -> t
(** [start_indent i ind] Start an indented grammar construct indented by at
    least [i] relative to its parent.

    If the aligmnent flag is set, indentation is ignored.

    Precondition: [0 <= incr]
*)


val end_indent: int -> t -> t -> t
(** [end_indent i ind0 ind] End the current indentation which has been started
    by [start_indent i ind0]. *)
