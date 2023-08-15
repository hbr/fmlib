open Fmlib_js


module Zone =
struct
    type t = int (* offset in minutes from utc, westward positive, eastward
                    negative *)

    let utc: t = 0

    let make (minutes: int): t =
        minutes

    let offset (zone: t): int =
        zone
end


type t = Date.t

let zero: t =
    Date.of_value (0.0)



let of_float (v: float): t =
    Date.of_value v



let to_float (t: t): float =
    Date.value t



let adapt (time: t) (zone: Zone.t): t =
    Date.(add (60000. *. float_of_int (zone_offset time - zone)) time)
        (* more westward is not yet that late *)


let year (time: t) (zone: Zone.t): int =
    Date.year (adapt time zone)


let month (time: t) (zone: Zone.t): int =
    Date.month (adapt time zone)


let day_of_month (time: t) (zone: Zone.t): int =
    Date.date (adapt time zone)


let hour (time: t) (zone: Zone.t): int =
    Date.hours (adapt time zone)


let minute (time: t) (zone: Zone.t): int =
    Date.minutes (adapt time zone)


let second (time: t) (zone: Zone.t): int =
    Date.seconds (adapt time zone)


let milli_second (time: t) (zone: Zone.t): int =
    Date.milliseconds (adapt time zone)
