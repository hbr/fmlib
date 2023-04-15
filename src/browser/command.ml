open Fmlib_js

type 'm t =
    | None
    | Task of ('m, Task.empty) Task.t
    | Batch of 'm t list


let none: 'm t =
    None



let batch (lst: 'm t list): 'm t =
    Batch lst



let perform (task: ('a, Task.empty) Task.t): 'm t =
    Task task



let attempt (f: ('a, 'e) result -> 'm) (task: ('a, 'e) Task.t): 'm t =
    Task (Task.make_succeed f task)



let execute (post: Base.Value.t -> unit) (dispatch: 'm -> unit) (cmd: 'm t): unit =
    let rec exe = function
        | None ->
            ()
        | Task task ->
            Task.run task post dispatch
        | Batch lst ->
            List.iter exe lst
    in
    exe cmd
