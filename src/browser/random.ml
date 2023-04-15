module R = Stdlib.Random

type 'a t = R.State.t -> 'a


let run (random: 'a t): 'a =
    random (R.State.make_self_init ())


let constant (a: 'a): 'a t =
    fun _ -> a

let (>>=) (m: 'a t) (f: 'a -> 'b t): 'b t =
    fun state -> f (m state) state (* state is mutable. *)

let ( let* ) = (>>=)


let map (f: 'a -> 'b) (m: 'a t): 'b t =
    let* a = m in
    constant (f a)


let int (bound: int): int t =
    fun state ->
    assert (0 < bound);
    R.State.int state bound


let float (bound: float): float t =
    fun state ->
    assert (0.0 <= bound);
    R.State.float state bound


let bool: bool t =
    R.State.bool


let choose (lst: 'a list): 'a t =
    let arr = Array.of_list lst in
    let len = Array.length arr in
    let* i = int len in
    assert (i < len);
    constant arr.(i)
