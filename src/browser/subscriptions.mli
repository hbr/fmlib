open Fmlib_js

type 'msg t


val empty: unit -> 'msg t


val update: ('msg -> unit) -> 'msg Subscription.t -> 'msg t -> 'msg t


val on_animation: float -> ('msg -> unit) -> 'msg t -> unit

val on_message: ('m -> unit) -> (unit -> 'm t) -> Base.Value.t -> unit
