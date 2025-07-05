open Fmlib_js

type 'm t =
    | None
    | Task of ('m, Task.empty) Task.t
    | Task_wo_message of (unit, Task.empty) Task.t
    | Set_ref of string * 'm Vdom.t
    | Batch of 'm t list


let none: 'm t =
    None



let batch (lst: 'm t list): 'm t =
    Batch lst




let set_refnode (name: string) (vd: 'm Vdom.t): 'm t =
    Set_ref (name, vd)


let map (f: 'a -> 'b) (cmd: 'a t): 'b t =
    let rec map = function
        | None ->
            None

        | Task task ->
            Task (Task.map f task)

        | Task_wo_message _ as cmd ->
            cmd

        | Set_ref (name, vd)->
            Set_ref (name, Vdom.map f vd)

        | Batch lst ->
            Batch (List.map map lst)
    in
    map cmd



let perform (task: ('m, Task.empty) Task.t): 'm t =
    Task task


let just_do (task: (unit, Task.empty) Task.t): 'm t =
    Task_wo_message task


let attempt (f: ('a, 'e) result -> 'm) (task: ('a, 'e) Task.t): 'm t =
    Task (Task.make_succeed f task)





let focus_with_info (id: string) (ok: 'm) (not_found: 'm): 'm t =
    attempt
        (function
            | Ok _ ->
                ok
            | Error _ ->
                not_found)
        (Task.focus id)




let blur_with_info (id: string) (ok: 'm) (not_found: 'm): 'm t =
    attempt
        (function
            | Ok _ ->
                ok
            | Error _ ->
                not_found)
        (Task.blur id)


let focus (id: string): 'm t =
    just_do
        Task.(make_succeed (fun _ -> ()) (focus id))


let blur (id: string): 'm t =
    just_do
        Task.(make_succeed (fun _ -> ()) (blur id))



let random (r: 'm Random.t): 'm t =
    perform (Task.random r)



let notify (millis: int) (m: 'm): 'm t =
    perform Task.(sleep millis m)


let log_string (s: string): 'm t =
    just_do Task.(log_string s)



let log_value (v: Base.Value.t): 'm t =
    just_do Task.(log_value v)


let now (f: Time.t -> 'm): 'm t =
    perform Task.(map f now)


let time_zone (f: Time.Zone.t -> 'm): 'm t =
    perform Task.(map f time_zone)


let file_text (file: File.t) (f: (string, Task.read_failed) result -> 'm): 'm t =
    attempt f (Task.file_text file)


let send_to_javascript (v: Base.Value.t): 'm t =
    just_do Task.(send_to_javascript v)


let set_reference (name: string) (vd: 'm Vdom.t): 'm t =
    Set_ref (name, vd)




let execute
        (post: Base.Value.t -> unit)
        (dispatch: 'm -> unit)
        (state: 's)
        (set_ref: string -> 'm Vdom.t -> 's -> unit)
        (cmd: 'm t)
    : unit
    =
    let rec exe = function
        | None ->
            ()

        | Task task ->
            Task.run task post dispatch

        | Task_wo_message task ->
            Task.run task post (fun _ -> ())

        | Set_ref (name, vd) ->
            set_ref name vd state

        | Batch lst ->
            List.iter exe lst
    in
    exe cmd
