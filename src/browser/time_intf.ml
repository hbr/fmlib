module type TIME =
sig
    (** Posix Time *)


    (** Posix time is the number of milliseconds passed since the beginning of
        the unix epoch i.e. since 1.1.1970 in utc i.e. universal coordinated
        time.

        In order to get the time in your time zone you need a time zone.
     *)


    (** Time zone *)
    module Zone:
    sig
        (** A time zone is the offset in minutes from utc. Time zones westward
            of utc get a positive offset, eastward of utc a negative offset.
         *)

        type t

        val utc: t

        val make: int -> t
        (** [make offset] Time zone [offset] minutes westward of utc.

            [make (-60)] is the zone of central european winter time. It is one
            hour eastward of utc.
         *)

        val offset: t -> int
        (** [offset zone] The offset of [zone] in minutes westward of utc. *)
    end



    type t (** Type of the posix time *)

    val zero: t
    (** [1.1.1970] in utc. *)



    val of_float: float -> t

    val to_float: t -> float



    (** In order to get a year, month, ... you need the utc time and the time
        zone you are in. *)


    val year:    t -> Zone.t -> int
    (** [year time zone] The year of [time] in [zone]. *)


    val month:   t -> Zone.t -> int
    (** [month time zone] The month of [time] in [zone].

        January is month [0].
    *)

    val day_of_month: t -> Zone.t -> int
    (** [day_of_month time zone] The day of the month of [time] in [zone].

        First day of the month is day [0].
    *)

    val hour:    t -> Zone.t -> int
    (** 0 - 23 *)

    val minute:  t -> Zone.t -> int
    (** 0 - 59 *)

    val second:  t -> Zone.t -> int
    (** 0 - 59 *)

    val milli_second:  t -> Zone.t -> int
    (** 0 - 999 *)
end
