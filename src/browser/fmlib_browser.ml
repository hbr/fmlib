module Random     = Random
module Time       = Time
module Task       = Task
module Value      = Value
module Event_flag = Event_flag
module Url        = Url
module File       = File
module Decoder    = Decoder
module Http       = Http
module Command    = Command
module Subscription = Subscription



let debug (str: string): unit =
    let open Fmlib_js.Base.Main in
    log_string str



let debug_value (v: Value.t): unit =
    let open Fmlib_js.Base.Main in
    log_value v




module Attribute = Attribute

module Html = Html

include Browser
