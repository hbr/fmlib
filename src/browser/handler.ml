open Fmlib_js



module Actual =
struct
    (* See Note [Functions with references] *)
    type 'e t = {
        ref: ('e -> unit) ref;
        real: 'e -> unit;
    }

    let make (handler: 'e -> unit): 'e t =
        let ref = ref handler in
        let f ref =
            fun e -> (! ref) e
        in
        { ref; real = f ref }


    let update (handler: 'e -> unit) (actual: 'e t): unit =
        actual.ref := handler

    let fire (actual: 'e t): 'e -> unit =
        actual.real
end



module Actuals (Key: Fmlib_std.Interfaces.SORTABLE) =
struct
    module Map  = Fmlib_std.Btree.Map (Key)
    module Dict = Dictionary.Make (Key)

    type ('e, 'a) t = ('e Actual.t * 'a) Map.t ref

    let empty (): ('e, 'a) t = ref Map.empty

    let set
            (make: 'v -> 'e -> unit)
            (add:  Key.t -> ('e -> unit) -> 'a)
            (dict: 'v Dict.t)
            (map: ('e, 'a) t)
        : unit
        =
        Dict.iter
            (fun key v ->
                 assert (Map.find_opt key !map = None);
                 let actual = Actual.make (make v)
                 in
                 let a = add key (Actual.fire actual) in
                 map := Map.add key (actual, a) !map
            )
            dict

    let update
            (make: 'v -> 'e -> unit)
            (add:  Key.t -> ('e -> unit) -> 'a)
            (remove: Key.t -> ('e -> unit) -> 'a -> unit)
            (d1: 'v Dict.t)
            (d2: 'v Dict.t)
            (map: ('e, 'a) t)
        : unit
        =
        Dict.diff
            (fun key v ->
                 (* Handler for [key] new in [d1]. *)
                 map :=
                     Map.update
                         key
                         (function
                             | None ->
                                 let actual = Actual.make (make v)
                                 in
                                 let a = add key (Actual.fire actual) in
                                 Some (actual, a)
                             | Some _ ->
                                 assert false (* Illegal call, handler is new in
                                                 [d1]. *)
                         )
                         !map
            )
            (fun key v ->
                 (* Handler for [key] needs update. It is present in [d1] and
                    [d2] *)
                 match Map.find_opt key !map with
                 | None ->
                     assert false (* Illegal call. Handler is present in [d1]
                                     and [d2]. *)
                 | Some (actual, _) ->
                     Actual.update (make v) actual
            )
            (fun key ->
                 (* Handler for [key] is not in [d1] but in [d2]. It has to be
                    removed. *)
                 map :=
                     Map.update
                         key
                         (function
                             | None ->
                                 assert false (* Illegal call. It is in [d2],
                                     therefore it has to be in
                                                 [map]. *)
                             | Some (actual, a) ->
                                 remove key (Actual.fire actual) a;
                                 None)
                         !map
            )
            d1
            d2
end






(* A virtual handler is a decoder with flags to stop propagation and prevent
   default handling.
*)
module Virtual =
struct
    type 'm t = Event_flag.stop * Event_flag.prevent * 'm Base.Decode.t

    let make_one
            (dispatch: 'm -> unit)
            ((Event_flag.Stop stop, Event_flag.Prevent prevent, decode): 'm t)
        : Event.t -> unit
        =
        fun event ->

        if stop then
            Event.stop_propagation event;

        if prevent then
            Event.prevent_default event;

        match decode Event.(value event) with
        | None ->
            let open Base.Main in
            log_string "Cannot decode event";
            log_value (Event.value event)
        | Some m ->
            dispatch m

    let make_list (dispatch: 'm -> unit) (lst: 'm t list): Event.t -> unit =
        fun event ->
        List.(iter
                  (fun v -> make_one dispatch v event)
                  (rev lst))

    let map (f: 'a -> 'b) ((stop, prevent, decode): 'a t): 'b t =
        stop,
        prevent,
        Base.Decode.map f decode
end







(* Set of real handlers handling javascript events fired on event targets. *)
module EventHs =
struct
    module Actuals = Actuals (String)
    module Dict    = Dictionary.Make (String)

    type t = (Event.t, unit) Actuals.t

    let empty = Actuals.empty

    let add target event_type handler =
        Event_target.add event_type handler target

    let set
            (target:   Event_target.t)
            (dispatch: 'm -> unit)
            (dict:     'm Virtual.t list Dict.t)
            (map:      t)
        : unit
        =
        Actuals.set
            (Virtual.make_list dispatch)
            (add target)
            dict
            map

    let update
            (target:   Event_target.t)
            (dispatch: 'm -> unit)
            (d1:       'm Virtual.t list Dict.t)
            (d2:       'm Virtual.t list Dict.t)
            (map:      t)
        : unit
        =
        Actuals.update
            (Virtual.make_list dispatch)
            (add target)
            (fun event_type handler _ ->
                Event_target.remove event_type handler target)
            d1
            d2
            map
end







module Timers =
struct
    module Actuals = Actuals (Int)
    module Dict    = Dictionary.Make (Int)

    type t = (Time.t, Timer.interval) Actuals.t

    let empty = Actuals.empty

    let of_list
            (dispatch: 'm -> unit)
            (lst: (Time.t -> 'm) list)
        : Time.t -> unit
        =
        fun time ->
        List.(iter (fun f -> dispatch (f time)) (rev lst))

    let make_timer (millis: int) (h: Time.t -> unit): Timer.interval =
        Timer.set_interval
            (fun () -> h (Date.now ()))
            millis

    let remove_timer
            (_: int) (_: Time.t -> unit) (timer: Timer.interval)
        : unit
        =
        Timer.clear_interval timer

    let set
            (dispatch: 'm -> unit)
            (dict: (Time.t -> 'm) list Dict.t)
            (map: t)
        : unit
        =
        Actuals.set
            (of_list dispatch)
            make_timer
            dict
            map


    let update
            (dispatch: 'm -> unit)
            (d1: (Time.t -> 'm) list Dict.t)
            (d2: (Time.t -> 'm) list Dict.t)
            (map: t)
        : unit
        =
        Actuals.update
            (of_list dispatch)
            make_timer
            remove_timer
            d1
            d2
            map
end



module Url_request =
struct
    type t = Event.t Actual.t option ref

    let empty (): t =
        ref None


    let target (): Event_target.t =
        Dom.Window.(event_target (get ()))

    let add (h: Event.t -> unit): unit =
        Event_target.add "click" h (target ())

    let remove (h: Event.t -> unit): unit =
        Event_target.remove "click" h (target ())


    let decode: Url.t Base.Decode.t =
        let open Base.Decode in
        let* tag  = field "target" (field "tagName" string) in
        let* href = field "target" (field "href"    string) in
        match Url.parse href with
        | None ->
            fail
        | Some url ->
            if tag <> "A" || tag <> "a" || not (Url.is_page url) then
                fail
            else
                return url


    let make (dispatch: 'm -> unit) (f: Url.t -> 'm) (event: Event.t): unit =
        match decode (Event.value event) with
        | None ->
            ()
        | Some url ->
            Event.prevent_default event;
            dispatch (f url)


    let set
            (dispatch: 'm -> unit)
            (virt: (Url.t -> 'm) option)
            (req: t)
        : unit
        =
        match virt with
        | None ->
            req := None
        | Some f ->
            let actual = Actual.make (make dispatch f) in
            add (Actual.fire actual);
            req := Some actual


    let update
            (dispatch: 'm -> unit)
            (virt1: (Url.t -> 'm) option)
            (virt2: (Url.t -> 'm) option)
            (req: t)
        : unit
        =
        match virt1, virt2, !req with
        | None, None, None ->
            ()

        | Some f1, None, None ->
            (* [f1] is new handler, not yet a current handler *)
            let actual = Actual.make (make dispatch f1) in
            add (Actual.fire actual);
            req := Some actual

        | Some f1, Some _, Some actual ->
            (* [f1] is handler which has to update the current handler. *)
            let handler = make dispatch f1 in
            Actual.update handler actual

        | None, Some _, Some actual ->
            (* No new handler, current handler has to be removed. *)
            remove (Actual.fire actual)

        | _, None, Some _ | _, Some _, None ->
            assert false (* Illegal call, [virt2] and [!req] are either both
                            empty or both present. *)
end







(*
    Note [Functions with references]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    The code has been unit tested with the following test case.

    type t = {
        ref: (unit -> int) ref;
        base: unit -> int;
    }

    let f1 (): int = 1
    let f2 (): int = 2

    let make (f: unit -> int): t =
        let ref = ref f in
        let g ref =
            fun () -> (! ref) ()
        in
        {
            ref;
            base = g ref;
        }

    let update (f: unit -> int) (h: t): unit =
        h.ref := f

    let%test _ =
        let h = make f1 in
        h.base () = 1


    let%test _ =
        let h = make f1 in
        update f2 h;
        Printf.printf "h.base () = %d\n" (h.base ());
        h.base () = 2
    *)
