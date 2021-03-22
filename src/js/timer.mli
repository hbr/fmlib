(** One shot timers and interval timers *)



type timer
(** Type of a one shot timer. *)


type interval
(** Type of an interval timer. *)


val set: (unit -> unit) -> int -> timer
(** [set callback millis] Start a one shot timer which runs [millis]
    milliseconds and after that executes [callback ()].

    The function returns a [timer] object which can be used to stop the timer.
*)

val clear: timer -> unit
(** [clear timer] Stop the timer [timer]. The call has no effect if [timer] has
    already fired.
*)



val set_interval: (unit -> unit) -> int -> interval
(** [set_interval callback millis] Start a timer which executes [callback ()]
    every [millis] milliseconds.

    The function returns an [interval] object which can be used to stop the
    timer.
*)


val clear_interval: interval -> unit
(** [clear_interval timer] Stop the interval timer [timer]. *)
