val sandbox:
    'state
    -> ('state -> 'msg Vdom.t)
    -> ('state -> 'msg -> 'state)
    -> unit


val element:
    string
    -> ('state * 'msg Command.t) Fmlib_js.Base.Decode.t
    -> ('state -> 'msg Vdom.t)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit




val application:
    string
    -> ('state * 'msg Command.t) Fmlib_js.Base.Decode.t
    -> ('state -> 'msg Vdom.t * string)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit
