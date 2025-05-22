open Fmlib_js


type empty = |

type http_error = [`Http_status of int | `Http_no_json | `Http_decode]

type not_found  = [`Not_found]

let absurd: empty -> 'a = function
    | _ -> .




type ('a, +'e) t =
    (Base.Value.t -> unit) -> (('a, 'e) result -> unit) -> unit



let continue (k: 'a -> unit) (a: 'a): unit =
    Assert_failure.attempt
        "Exception in task execution"
        (fun () -> k a)
        (fun () -> ())


let run (task: ('a, empty) t) (post: Base.Value.t -> unit) (k: 'a -> unit): unit =
    task
        post
        (function
            | Ok a -> continue k a
            | _ -> .
        )


let succeed (a: 'a): ('a, 'e) t =
    fun _ k ->
    continue k (Ok a)


let return: 'a -> ('a, 'e) t =
    succeed


let fail (e: 'e): ('a, 'e) t =
    fun _ k ->
    continue k (Error e)



let result (r: ('a, 'e) result): ('a, 'e) t =
    fun _ k ->
    continue k r



let (>>=) (m: ('a, 'e) t) (f: 'a -> ('b, 'e) t): ('b, 'e) t =
    fun post k ->
    m
        post
        (function
            | Ok a ->
                f a post k
            | Error e ->
                continue k (Error e))

let ( let* ) = (>>=)



let map (f: 'a -> 'b) (m: ('a, 'e) t): ('b, 'e) t =
    let* a = m in
    return (f a)



let make_succeed (f: ('a, 'e) result -> 'b) (m: ('a, 'e) t): ('b, empty) t =
    fun post k ->
    m post (fun res -> continue k (Ok (f res)))



let parallel
        (start: 'accu)
        (next: 'a -> 'accu -> 'accu)
        (lst: ('a, empty) t list)
    : ('accu, empty) t
    =
    let n_ref    = ref (List.length lst)
    and accu_ref = ref start
    in
    fun post k ->
        let terminate () =
            if !n_ref = 0 then
                k (Ok !accu_ref)
        in
        let k0: ('a, empty) result -> unit  = function
            | Ok a ->
                assert (!n_ref <> 0);
                n_ref := !n_ref - 1;
                accu_ref := next a !accu_ref;
                terminate ()
            | _ ->
                .
        in
        terminate ();
        List.iter
            (fun task -> task post k0)
            lst



let log_string (s: string): (unit, 'e) t =
    fun _ k ->
    Base.Main.log_string s;
    continue k (Ok ())



let log_value (v: Base.Value.t): (unit, 'e) t =
    fun _ k ->
    Base.Main.log_value v;
    continue k (Ok ())



let sleep (ms: int) (a: 'a) : ('a, 'e) t =
    fun _ k ->
    ignore (
        Timer.set
            (fun () -> continue k (Ok a))
            ms
    )


let next_tick (a: 'a): ('a, 'e) t =
    sleep 0 a



let send_to_javascript (v: Base.Value.t): (unit, 'e) t =
    fun post k ->
    post v;
    continue k (Ok ())



let focus (id: string): (unit, not_found) t =
    fun _ k ->
    match Dom.(Document.find id Window.(document (get ()))) with
    | None ->
        k (Error `Not_found)
    | Some el ->
        Dom.Element.focus el;
        continue k (Ok ())


let blur (id: string): (unit, not_found) t =
    fun _ k ->
    match Dom.(Document.find id Window.(document (get ()))) with
    | None ->
        continue k (Error `Not_found)
    | Some el ->
        Dom.Element.blur el;
        continue k (Ok ())




let random (rand: 'a Random.t): ('a, 'e) t =
    fun _ k ->
    continue k (Ok (Random.run rand))



let http_text
        (meth: string)
        (url: string)
        (headers: (string * string) list)
        (body: string)
    : (string, http_error) t
    =
    fun _ k ->
    let req = Http_request.make meth url headers body in
    let handler _ =
        assert (Http_request.ready_state req = 4);
        let status = Http_request.status req in
        if status >= 300 then (* not ok *)
            continue k (Error (`Http_status status))
        else
            continue k (Ok (Http_request.response_text_string req))
    in
    Event_target.add
        "loadend"
        handler
        (Http_request.event_target req)


let http_json
        (meth: string)
        (url: string)
        (headers: (string * string) list)
        (body: string)
        (decode: 'a Base.Decode.t)
    : ('a, http_error) t
    =
    fun _ k ->
    let req = Http_request.make meth url headers body in
    let handler _ =
        assert (Http_request.ready_state req = 4);
        let status = Http_request.status req in
        if status >= 300 then (* not ok *)
            continue k (Error (`Http_status status))
        else
            match
                Base.Value.parse (Http_request.response_text_value req)
            with
            | None ->
                continue k (Error `Http_no_json)
            | Some v ->
                match decode v with
                | None ->
                    continue k (Error `Http_decode)
                | Some a ->
                    continue k (Ok a)
    in
    Event_target.add
        "loadend"
        handler
        (Http_request.event_target req)


let now: (Time.t, 'e) t =
    fun _ k ->
    continue k (Ok (Date.now ()))


let time_zone: (Time.Zone.t, 'e) t =
    fun _ k ->
    continue k (Ok (Date.(zone_offset (now ()))))
