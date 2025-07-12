type 'a t = 'a option

let return (a: 'a): 'a t =
    Some a

let fail: 'a t =
    None


let (let* ) (m: 'a t) (f: 'a -> 'b t): 'b t =
    match m with
    | Some a ->
        f a
    | None ->
        None


let (>>=) = (let* )



let (<*>) (f: ('a -> 'b) t) (a: 'a t): 'b t =
    match f, a with
    | Some f, Some a ->
        Some (f a)
    | _, _ ->
        None



let map (f: 'a -> 'b): 'a t -> 'b t = function
    | None ->
        None
    | Some a ->
        Some (f a)



let to_list (m: 'a t): 'a list =
    match m with
    | Some a ->
        [a]
    | None ->
        []


let to_result (err: 'e): 'a t -> ('a, 'e) result = function
    | None ->
        Error err
    | Some a ->
        Ok a
