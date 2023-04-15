open Fmlib_js.Base

type 'msg handler = 'msg Handler.Virtual.t

type 'msg t =
    | Style of string * string
    | Property of string * Value.t
    | Attribute of string * string
    | Handler of string * 'msg handler


let style (key: string) (value: string): 'a t =
    Style (key, value)

let property (key: string) (value: Value.t): 'a t =
    Property (key, value)

let attribute (key: string) (value: string): 'a t =
    Attribute (key, value)

let handler
        (key: string)
        (stop: Event_flag.stop)
        (prevent: Event_flag.prevent)
        (decode: 'msg Decode.t)
    : 'a t
    =
    Handler (key, (stop, prevent, decode))
