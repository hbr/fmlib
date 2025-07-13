module type FLAG =
sig
    (** Event flags to stop propagation and prevent default action. *)

    type prevent (** Flag type to prevent default action. *)

    type stop (** Flag type to stop propagation. *)

    val prevent:    prevent
    (** Prevent default handling. *)

    val no_prevent: prevent
    (** Do not prevent default handling. *)


    val stop:    stop
    (** Stop event propagation. *)

    val no_stop: stop
    (** Do not stop event propagation. *)
end
