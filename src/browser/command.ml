open Fmlib_js

type 'm t =
    | None
    | Task of ('m, Task.empty) Task.t
    | Task_wo_message of (unit, Task.empty) Task.t
    | Batch of 'm t list


let none: 'm t =
    None



let batch (lst: 'm t list): 'm t =
    Batch lst



let perform (task: ('a, Task.empty) Task.t): 'm t =
    Task task


let just_do (task: (unit, Task.empty) Task.t): 'm t =
    Task_wo_message task



let attempt (f: ('a, 'e) result -> 'm) (task: ('a, 'e) Task.t): 'm t =
    Task (Task.make_succeed f task)


let map (f: 'a -> 'b) (cmd: 'a t): 'b t =
    let rec map = function
        | None ->
            None
        | Task task ->
            Task (Task.map f task)
        | Task_wo_message _ as cmd ->
            cmd
        | Batch lst ->
            Batch (List.map map lst)
    in
    map cmd




let execute (post: Base.Value.t -> unit) (dispatch: 'm -> unit) (cmd: 'm t): unit =
    let rec exe = function
        | None ->
            ()

        | Task task ->
            Task.run task post dispatch

        | Task_wo_message task ->
            Task.run task post (fun _ -> ())

        | Batch lst ->
            List.iter exe lst
    in
    exe cmd
