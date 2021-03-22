open Js_of_ocaml

type t = Js.date Js.t


let now (): t =
    new%js Js.date_now


let make
        (year: int)
        (month: int)
        (day: int)
        (hour: int)
        (minute: int)
        (second: int)
        (milli: int)
    : t
    =
    new%js Js.date_ms year month day hour minute second milli


let of_value (v: float): t =
    new%js Js.date_fromTimeValue v


let value (d: t): float =
    d##valueOf


let zone_offset (d: t): int =
    d##getTimezoneOffset


let add (ms: float) (d: t): t =
    of_value (value d +. ms)


let day (d: t): int =
    d##getDay


let date (d: t): int =
    d##getDate


let month (d: t): int =
    d##getMonth


let year (d: t): int =
    d##getFullYear


let hours (d: t): int =
    d##getHours


let minutes (d: t): int =
    d##getMinutes


let seconds (d: t): int =
    d##getSeconds


let milliseconds (d: t): int =
    d##getMilliseconds
