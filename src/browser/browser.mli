val sandbox:
    'state
    -> ('state -> 'msg Vdom.t)
    -> ('state -> 'msg -> 'state)
    -> unit



val sandbox_plus:
    'state
    -> ('state -> 'msg Vdom.t)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state)
    -> unit



val element:
    string
    -> ('state * 'msg Command.t) Fmlib_js.Base.Decode.t
    -> ('state -> 'msg Vdom.t)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit



val basic_application:
    'state
    -> 'msg Command.t
    -> ('state -> 'msg Vdom.t * string)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> unit




val application:
    string
    -> (Url.t -> 'msg Navigation.key -> ('state * 'msg Command.t) Fmlib_js.Base.Decode.t)
    -> ('state -> 'msg Vdom.t * string)
    -> ('state -> 'msg Subscription.t)
    -> ('state -> 'msg -> 'state * 'msg Command.t)
    -> (Navigation.url_request -> 'msg)
    -> (Url.t -> 'msg)
    -> unit
