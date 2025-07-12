open Fmlib_js

module String_map = Fmlib_std.Btree.Map (String)
module Int_map  = Fmlib_std.Btree.Map (Int)

module Intionary  = Dictionary.Make (Int)

module Subs =
struct
    type 'm handler = 'm Handler.Virtual.t

    type 'm t = {

        window: 'm handler list Dictionary.t;

        timers: (Time.t -> 'm) list Intionary.t;

        animation: (Time.t -> 'm) option;

        message: 'm Base.Decode.t option;

        url_request: (Url.t -> 'm) option
    }


    let empty: 'm t = {
        window      = Dictionary.empty;
        timers      = Intionary.empty;
        animation   = None;
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


            | Animation callback ->
                {subs with
                 animation = Some callback;
                }

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



let empty (): 'm t =
    let open Handler
    in
    { subs        = Subs.empty;
      window      = EventHs.empty ();
      timers      = Timers.empty ();
      url_request = Url_request.empty ();
    }





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



let on_animation (time: float) (dispatch: 'm -> unit) (s: 'm t): unit =
    match s.subs.animation with
    | None ->
        ()

    | Some f ->
        dispatch (f (Time.of_float time))



let on_message (dispatch: 'm -> unit) (s: unit -> 'm t): Base.Value.t -> unit =
    fun v ->
    match (s ()).subs.message with
    | None ->
        ()

    | Some decode ->
        match decode v with
        | None ->
            Base.Main.log_string "Cannot decode message from javascript"

        | Some m ->
            dispatch m
