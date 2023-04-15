open Fmlib_js

module String_map = Fmlib_std.Btree.Map (String)
module Int_map  = Fmlib_std.Btree.Map (Int)

module Intionary  = Dictionary.Make (Int)
module Dictionary = Dictionary.Make (String)

module Subs =
struct
    type 'm handler = 'm Handler.Virtual.t

    type 'm t = {

        window: 'm handler list Dictionary.t;

        timers: (Time.t -> 'm) list Intionary.t;

        message: 'm Base.Decode.t option;

        url_request: (Url.t -> 'm) option
    }


    let empty: 'm t = {
        window      = Dictionary.empty;
        timers      = Intionary.empty;
        message     = None;
        url_request = None;
    }


    let make (sub: 'm Subscription.t): 'm t =
        let open Subscription in
        let rec make subs = function
            | None ->
                subs

            | Batch lst ->
                List.fold_left
                    make
                    subs
                    lst
            | Window (event_type, handler) ->
                {subs with
                 window =
                     Dictionary.set
                         event_type
                         (function
                             | None ->
                                 [handler]
                             | Some lst ->
                                 handler :: lst
                         )
                         subs.window}

            | Interval_timer (millis, callback) ->
                {subs with
                 timers =
                     Intionary.set
                         millis
                         (function
                             | None ->
                                 [callback]
                             | Some lst ->
                                 callback :: lst
                         )
                         subs.timers}

            | Message decode ->
                {subs with
                 message =
                     match subs.message with
                     | None ->
                         Some decode
                     | Some _ ->
                         subs.message}

            | Url_request f ->
                {subs with
                 url_request =
                     match subs.url_request with
                     | None ->
                         Some f
                     | Some _ ->
                         subs.url_request}
        in
        make empty sub
end








type 'm t = {
    subs:   'm Subs.t;
    window: Handler.EventHs.t;
    timers: Handler.Timers.t;
    url_request: Handler.Url_request.t
}




let make (dispatch: 'm -> unit) (sub: 'm Subscription.t): 'm t =
    let subs   = Subs.make sub in
    let open Handler in

    let window = EventHs.empty () in
    EventHs.set
        Fmlib_js.Dom.Window.(event_target (get ()))
        dispatch
        subs.window
        window;

    let timers = Timers.empty () in
    Timers.set dispatch subs.timers timers;

    let url_request = Url_request.empty () in
    Url_request.set dispatch subs.url_request url_request;
    { subs; window; timers; url_request }



let update (dispatch: 'm -> unit) (sub: 'm Subscription.t) (s: 'm t): 'm t =
    let subs   = Subs.make sub in
    let open Handler in
    EventHs.update
        Fmlib_js.Dom.Window.(event_target (get ()))
        dispatch
        subs.window
        s.subs.window
        s.window;
    Timers.update dispatch subs.timers s.subs.timers s.timers;
    Url_request.update
        dispatch
        subs.url_request
        s.subs.url_request
        s.url_request;
    { s with subs }
