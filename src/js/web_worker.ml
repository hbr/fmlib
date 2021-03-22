open Js_of_ocaml
open Base


(* Helper functions
 * ================
 *)


let decode_callback (err: unit -> string) (cb: Value.t): Value.t -> unit =
    match Decode._function cb with
    | None ->
        Main.raise_js (err ())
    | Some cb ->
        fun v -> cb [|v|] |> ignore


let send_message
        (err: unit -> string)
        (msg: Value.t) (dec: 'msg Decode.t)
        (cb: 'msg -> unit)
    : unit
    =
    match dec msg with
    | None ->
        let open Main in
        log_string (err ());
        log_value msg
    | Some msg ->
        cb msg


let do_async (f: unit -> unit): unit =
    Timer.set f 0 |> ignore



(* Creator Code
   ============
*)



class type worker =
object
    method postMessage: 'msg -> unit Js.meth
    method terminate: unit Js.meth
end



type t = worker Js.t


let start (url: string) (dec: 'msg Decode.t) (cb: 'msg -> unit): t =
    let open Main in
    let w =
        new_global "Worker" [|Value.string url|]
    and cb event =
        send_message
            (fun _ -> "cannot decode message from " ^ url ^ "\"")
            (Event.value event)
            dec
            cb
    in
    Event_target.add "message" cb (Obj.magic w); (* [w] is an event target. *)
    Obj.magic w (* [w] is a worker, because it has been construced
                   by [Worker]. *)




let post_message (msg: Value.t) (w: t): unit =
    w##postMessage msg



let terminate (w: t): unit =
    w##terminate




(* Implementation Code
   ===================
*)


type 'msg worker_function = (Value.t -> unit) -> 'msg -> unit

let make (decode: 'msg Decode.t) (f: 'msg worker_function): unit =
    let open Main in
    let post =
        match get_global "postMessage" with
        | None ->
            raise_js "webworker: <postMessage> function not available"
        | Some post ->
            post
    in
    let post =
        decode_callback
            (fun _ -> "webworker: <postMessage> is not a function")
            post
    in
    let f = f post
    in
    make_global
        "onmessage"
        (Value.function1
             (fun msg ->
                  send_message
                      (fun _ -> "webworker: cannot decode message")
                      msg
                      decode
                      f;
                  Value.undefined))






(* Simulation
 * ==========
 *)


module Simulate =
struct
    type t = (Value.t -> unit) option ref


    let start
            (dec: 'rcv Decode.t) (cb: 'rcv -> unit)
            (worker_decode: 'msg Decode.t) (worker: 'msg worker_function)
        : t
        =
        let post_to_creator v =
            send_message
                (fun _ -> "main: cannot decode message from worker")
                v dec cb
        in
        let post_to_creator v =
            do_async (fun _ -> post_to_creator v)
        in
        let f = worker post_to_creator
        in
        let post_to_worker v =
            do_async
                (fun _ ->
                     send_message
                         (fun _ -> "webworker: cannot decode message")
                         v worker_decode f)
        in
        ref (Some post_to_worker)


    let post_message (msg: Value.t) (w: t): unit =
        match !w with
        | None ->
            Main.log_string "worker has already been terminated"
        | Some post ->
            post msg


    let terminate (w: t): unit =
        w := None
end





let simulate_js
        (name: string)
        (decode: 'msg Decode.t)
        (wfun: 'msg worker_function)
    : unit
    =
    let open Main in
    (fun post ->
         let post_to_creator =
             decode_callback
                 (fun _ -> "webworker: <postMessage> is not a funtion")
                 post
         in
         let w =
             Simulate.start
                 (fun v -> Decode.return v v)
                 post_to_creator
                 decode
                 wfun
         in
         let post_to_worker msg =
              Simulate.post_message msg w;
              Value.undefined
         and terminate _ =
             Simulate.terminate w;
             Value.undefined
         in
         Value._object [|
             "postMessage", Value.function1 post_to_worker;
             "terminate",   Value.function1 terminate
         |]
    )
    |> Value.function1
    |> make_global name
