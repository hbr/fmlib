(** The allowed indentations: Helper module for indentation sensitive parsing.

*)


type expectation =
    | Indent of int
    (** [Indent n] An indentation of at least [n] columns is expected. *)

    | Align of int (** [Align n] Start at colmun [n] is expected. *)

    | Align_between of int * int (** [Align_between a b] Start between the
                                     columns [a] and [b] is expected. *)
(** The expected indentiation. *)


type violation = expectation
(** @deprecated Use [expectation]! *)


val group:
    ('a * expectation option) list
    -> (expectation option * 'a list) list
(** [group lst] Group the list of expectations.

    Failed expectations with the same indentation expectation (or not
    indentation expectation) are grouped into one list. The sequence is not
    changed.
*)



type t
(** Allowed indentations *)



val expectation: t -> expectation option
(** [expectation ind] The expected indentation or alignment. Returns [None] if
    all positions are allowed. *)


val initial: t
(** Initially all indentations 0,1,... are allowed and no alignment is required.

*)


val check_position: int -> t -> expectation option
(** [check_position col ind] Return a violated expectation, if [pos] is not an
    allowed indentation position. Otherwise return [None]. *)


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


val end_align: t -> t -> t
(** [end_align ind0 ind] End the aligned sequence i.e. handle the corner case
    that the aligned sequence is empty.
 *)


val start_indent: int -> t -> t
(** [start_indent i ind] Start an indented grammar construct indented by at
    least [i] relative to its parent.

    If the aligmnent flag is set, indentation is ignored.

    Precondition: [0 <= incr]
*)


val end_indent: int -> t -> t -> t
(** [end_indent i ind0 ind] End the current indentation which has been started
    with and indentation of [i] columns relative to [ind0]. *)
