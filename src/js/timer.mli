type timer
type interval

val set: (unit -> unit) -> int -> timer
val clear: timer -> unit


val set_interval: (unit -> unit) -> int -> interval
val clear_interval: interval -> unit
