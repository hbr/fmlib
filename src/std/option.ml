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


let map (f: 'a -> 'b) (m: 'a t): 'b t =
    let* a = m in
    return (f a)



let to_list (m: 'a t): 'a list =
    match m with
    | Some a ->
        [a]
    | None ->
        []
