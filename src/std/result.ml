type ('a, 'e) t = ('a, 'e) result


let return (a: 'a): ('a, 'e) t =
    Ok a


let fail (e: 'e): ('a, 'e) t =
    Error e



let to_option (r: ('a, _) t): 'a option =
    match r with
    | Ok a ->
        Some a
    | Error _ ->
        None


let (>>=) (m: ('a, 'e) t) (f: 'a -> ('b, 'e) t): ('b, 'e) t =
    match m with
    | Ok a ->
        f a
    | Error e ->
        Error e

let ( let* ) = (>>=)


let map (f: 'a -> 'b): ('a, 'e) t -> ('b, 'e) t = function
    | Ok a ->
        Ok (f a)

    | Error e ->
        Error e



let map_error (f: 'e -> 'f): ('a, 'e) t -> ('a, 'f) t = function
    | Ok a ->
        Ok a

    | Error e ->
        Error (f e)


let get: ('a, Void.t) t -> 'a = function
    | Ok a ->
        a

    | Error e ->
        Void.absurd e





module Monad (E: Interfaces.ANY) =
struct
    type 'a t = ('a, E.t) result

    let return = return

    let fail = fail

    let to_option = to_option

    let (>>=)  = (>>=)

    let ( let* ) = (>>=)
end



(* Unit tests *)
(*****************************************)

type 'a r = ('a, string) result

let add (a: int r) (b: int r): int r =
    let* x = a in
    let* y = b in
    Ok (x + y)

let divide (a: int r) (b: int r): int r =
    let* x = a in
    let* y = b in
    if y = 0 then
        Error "Division by Zero"
    else
        Ok (x / y)

let%test _ =
    add (Ok 1) (divide (Ok 2) (Ok 0))
    =
    Error "Division by Zero"

let%test _ =
    add (Ok 1) (divide (Ok 10) (Ok 2))
    =
    Ok 6
