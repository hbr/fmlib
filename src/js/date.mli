(** The javascript date object *)

(**
    The javascript date object is the number of milliseconds passed from
    1.1.1970 UTC. The date object has no time zone. Its value of milliseconds is
    always since 1.1.1970 UTC.

    All access functions (except for [value]) like [day], [hours], ... etc.
    return the values valid in the timezone of your system. I.e. [hours
    (of_value 0.)] does not return [0] but the hour in the timezone of your
    system at 1.1.1970 UTC.

    If you have a date object [d] and want the day, hour, ... in another
    timezone which has [offset0] to UTC you can do the following:

    {[
        let d0 =
            add
                ((Float.of_int (zone_offset d - offset0)) *. 60000)
                d
    ]}

    Then [day d0], [hours d0], ... are the values in the desired timezone.
*)

type t
(* Type of the date object. *)


val now: unit -> t
(** Construct a new date object at the current time. *)


val value: t -> float
(** The number of milliseconds passed since 1.1.1970 UTC. *)


val of_value: float -> t
(** [of_value millis] Construct a new date object pointing to 1.1.1970 UTC +
    [ms] milliseconds.
*)

val zone_offset: t -> int
(** Timezone offset in minutes from UTC. Westward positive, eastward negative.
*)


val add: float -> t -> t
(** [add ms date] Add [ms] milliseconds to the current time and return the new
    date object.
*)


val make: int -> int -> int -> int -> int -> int -> int -> t
(** [make year month day hour minute second millis] Make a date object for the
    current time zone. *)


val day: t -> int
(** The day of the week.

    sunday .. saturday  ~>  0 .. 6
*)

val date: t -> int
(** The date in the month starting with [1]. *)

val month: t -> int
(** The month (1-12) *)

val year:  t -> int
(** The year. *)

val hours: t -> int
(** The hour (0-23). *)

val minutes: t -> int
(** The minute (0-59). *)

val seconds: t -> int
(** The second (0-59). *)

val milliseconds: t -> int
(** The the millisecond (0-999). *)
