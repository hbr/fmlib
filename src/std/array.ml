include Stdlib.Array


type 'a t = 'a array

let is_empty (xs: 'a t): bool =
    length xs = 0


let valid_index (i: int) (xs: 'a t): bool =
    0 <= i && i < length xs


let has_some (xs: 'a t): bool =
    0 < length xs


let first (xs: 'a t): 'a =
    assert (has_some xs);
    get xs 0


let last (xs: 'a t): 't =
    let len = length xs in
    assert (0 < len);
    get xs (len - 1)


let push (x: 'a) (xs: 'a array): 'a array =
    let len = length xs in
    let xs_new = make (len + 1) x in
    blit xs 0 xs_new 0 len;
    xs_new


let push_front (x: 'a) (xs: 'a array): 'a array =
    let len = length xs in
    let xs_new = make (len + 1) x in
    blit xs 0 xs_new 1 len;
    xs_new


let insert (i: int) (x: 'a) (xs: 'a array): 'a array =
    assert (0 <= i);
    assert (i <= length xs);
    (* 0 1 ... (i-1)   i ... (len-1)
                     ^ insert here
    *)
    let len = length xs in
    let arr = make (len + 1) x in
    blit xs 0 arr 0 i;
    blit xs i arr (i + 1) (len - i);
    arr


let replace (i: int) (x: 'a) (xs: 'a array): 'a array =
    assert (0 <= i);
    assert (i < length xs);
    (* 0 1 ... i ... (len-1)
               ^ replace
    *)
    let len = length xs in
    let arr = make len x in
    blit xs 0 arr 0 i;
    blit xs (i + 1) arr (i + 1) (len - (i + 1));
    arr



let remove (i: int) (xs: 'a array): 'a array =
    assert (0 <= i);
    assert (i < length xs);
    (* 0 1 ... i  (i + 1) ... (len-1)
               ^ remove
    *)
    let len = length xs in
    assert (0 < len);
    let arr = make (len - 1) (get xs 0) in
    blit xs 0 arr 0 i;
    blit xs (i + 1) arr i (len - (i + 1));
    arr


let remove_first (xs: 'a array): 'a array =
    let len = length xs in
    assert (0 < len);
    sub xs 1 (len - 1)



let remove_last (xs: 'a array): 'a array =
    let len = length xs in
    assert (0 < len);
    sub xs 0 (len - 1)



let binsearch
        (compare: 'key -> 'key -> int)
        (key_of: 'a -> 'key)
        (key: 'key)
        (arr: 'a array)
    : int * bool =
    (* Search the position of [key] in [arr] which is sorted without
       duplicates.

        Result: i, exact_flag with [key <= project arr.(i)]

        If [exact_flag] is set, the key in position [i] is exactly [key],
        otherwise the key in position [i] is strictly greater than [key].

        Corner case: [i = length arr, exact_flag = false]. This corresponds to a
        fictitious key of [+ infinity] at position [length arr].

        Precondition:
            The array must be sorted and does not contain duplicates.
    *)
    let len = length arr
    in
    if len = 0 then
        len, false
    else if len = 1 then
        let cmp = compare key (get arr 0 |> key_of) in
        if cmp <= 0 then
            0, cmp = 0
        else
            len, false
    else
        let rec search lower upper =
            (* Invariant:

                0 <= lower < upper < len
                arr.(lower) < key < arr.(upper)
            *)
            if lower + 1 = upper then
                upper, false
            else
                let mid = lower + (upper - lower) / 2 in
                assert (lower < mid);
                assert (mid < upper);
                let cmp = compare key (get arr mid |> key_of) in
                if cmp = 0 then
                    mid, true
                else if cmp < 0 then
                    search lower mid
                else
                    search mid upper
        in
        let lower, upper = 0, len - 1 in
        let cmp = compare key (get arr lower |> key_of) in
        if cmp <= 0 then
            (* key is less or equal the first element *)
            lower, cmp = 0
        else
            (* key is greater than the first element *)
            let cmp = compare key (get arr upper |> key_of) in
            if cmp < 0 then
                (* invariant for [search] satisfied. *)
                search lower upper
            else if cmp = 0 then
                (* exact match with the last element *)
                upper, true
            else
                (* key is greater than all elements *)
                len, false






module Map (Key: Interfaces.SORTABLE) =
struct
    type key = Key.t

    type 'a t = (Key.t * 'a) array


    let cardinal (map: 'a t): int =
        length map


    let is_empty (map: 'a t): bool =
        cardinal map = 0


    let bindings (map: 'a t): (Key.t * 'a) list =
        to_list map


    let fold_left (f: 'a -> Key.t -> 'b -> 'a) (start: 'a) (map: 'b t): 'a =
        Stdlib.Array.fold_left
            (fun a (key, value) -> f a key value)
            start
            map

    let fold_right (f: 'a -> Key.t -> 'b -> 'a) (start: 'a) (map: 'b t) =
        Stdlib.Array.fold_right
            (fun (key, value) result -> f result key value)
            map
            start


    let index_of (key: Key.t) (map: 'a t): int option =
        let len = length map in
        let i, exact = binsearch Key.compare fst key map in
        if i = len || not exact then
            None
        else
            Some i

    let pair (i: int) (map: 'a t): Key.t * 'a =
        assert (i < cardinal map);
        get map i


    let find_opt (key: Key.t) (map: 'a t): 'a option =
        Option.map
            (fun i -> snd (get map i))
            (index_of key map)


    let mem (key: Key.t) (map: 'a t): bool =
        index_of key map <> None


    let empty: 'a t =
        [||]


    let singleton (key: Key.t) (value: 'a): 'a t =
        [| key, value |]



    let add (key: Key.t) (value: 'a) (map: 'a t): 'a t =
        let i,exact = binsearch Key.compare fst key map in
        if exact then
            replace i (key, value) map
        else
            insert i (key, value) map



    let update (key: Key.t) (f: 'a option -> 'a option) (map: 'a t): 'a t =
        let i, exact = binsearch Key.compare fst key map in
        if exact then
            match f (Some (get map i |> snd)) with
            | None ->
                remove i map
            | Some value ->
                replace i (key, value) map
        else
            match f None with
            | None ->
                map
            | Some value ->
                insert i (key, value) map


    let remove (key: Key.t) (map: 'a t): 'a t =
        let i, exact = binsearch Key.compare fst key map in
        if exact then
            remove i map
        else
            map
end





module Set (Key: Interfaces.SORTABLE) =
struct
    module M =  Map (Key)

    type item = Key.t

    type t = unit M.t

    let cardinal (set: t): int =
        M.cardinal set


    let is_empty = M.is_empty


    let fold_left (f: 'a -> Key.t -> 'a) (start: 'a) (set: t): 'a =
        M.fold_left
            (fun res key _ -> f res key)
            start
            set


    let fold_right (f: 'a -> Key.t -> 'a) (start: 'a) (set: t): 'a =
        M.fold_right
            (fun res key _ -> f res key)
            start
            set


    let elements (set: t): Key.t list =
        fold_right
            (fun lst key -> key :: lst)
            []
            set


    let element (i: int) (set: t): Key.t =
        assert (0 <= i);
        assert (i < cardinal set);
        M.pair i set |> fst


    let index_of = M.index_of


    let empty = M.empty


    let singleton (e: Key.t): t =
        M.singleton e ()


    let mem = M.mem


    let add (e: Key.t) (set: t): t =
        M.add e () set


    let remove = M.remove
end










(* Unit Tests
 * ==========
 *)


(* Binary search *)
let%test _ =
    binsearch Int.compare Fun.id 100 [||] = (0, false)



let%test _ =
    binsearch Int.compare Fun.id 99 [|100|] = (0, false)



let%test _ =
    binsearch Int.compare Fun.id 100 [|100|] = (0, true)



let%test _ =
    binsearch Int.compare Fun.id 101 [|100|] = (1, false)





(* Set *)
module SetInt = Set (Int)

let insert_downward (lower: int) (beyond: int) (set: SetInt.t): SetInt.t =
    let rec insert i set =
        if i = lower then
            set
        else
            let i = i - 1 in
            insert i (SetInt.add i set)
    in
    insert beyond set

let insert_upward (lower: int) (beyond: int) (set: SetInt.t): SetInt.t =
    let rec insert i set =
        if i = beyond then
            set
        else
            insert (i + 1) (SetInt.add i set)
    in
    insert lower set



let%test _ =
    insert_upward 0 3 SetInt.empty = [|0,(); 1,(); 2,()|]


let%test _ =
    insert_downward 0 3 SetInt.empty = [|0,(); 1,(); 2,()|]


let%test _ =
    insert_downward 0 3 SetInt.empty |> SetInt.remove 1 = [|0,(); 2,()|]


let%test _ =
    insert_downward 0 3 SetInt.empty  |> SetInt.remove 4 = [|0,(); 1,(); 2,()|]


let%test _ =
    (insert_upward 0 3 SetInt.empty |> insert_downward 0 3)
    =
    [|0,(); 1,(); 2,()|]


let%test _ =
    (insert_upward 0 3 SetInt.empty |> insert_downward 3 6)
    =
    [|0,(); 1,(); 2,(); 3,(); 4,(); 5,()|]


let%test _ =
    (insert_downward 0 3 SetInt.empty |> SetInt.remove 0)
    =
    [|1,(); 2,()|]


let%test _ =
    let set = insert_downward 0 3 SetInt.empty in
    SetInt.index_of 3 set = None


let%test _ =
    let set = insert_downward 0 3 SetInt.empty in
    SetInt.index_of 0 set = Some 0
