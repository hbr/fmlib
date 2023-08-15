open Fmlib_std

module type DICT =
sig
    type key

    type _ t

    val empty: 'a t

    val map: ('a -> 'b) -> 'a t -> 'b t

    val find_opt: key -> 'a t -> 'a option

    val add: key -> 'a -> 'a t -> 'a t

    val set: key -> ('a option -> 'a) -> 'a t -> 'a t

    val of_list: (key * 'a) list -> 'a t

    val fold: ('accu -> key -> 'a -> 'accu) -> 'accu -> 'a t -> 'accu

    val iter: (key -> 'a -> unit) -> 'a t -> unit

    val diff:
        (key -> 'a -> unit)
        -> (key -> 'a -> unit)
        -> (key -> unit)
        -> 'a t
        -> 'a t
        -> unit
end


module Make (Key: Interfaces.SORTABLE) =
struct
    module Map   = Fmlib_std.Btree.Map (Key)

    type 'a t = {
        arr: (Key.t * 'a ) Array.t;
        map: int Map.t;
    }


    let empty: 'a t = {
        arr = [||];
        map = Map.empty;
    }


    let map (f: 'a -> 'b) (d: 'a t): 'b t =
        {
            d with
            arr = Array.map (fun (key, a) -> (key, f a)) d.arr
        }



    let find_opt (key: Key.t) (d: 'a t): 'a option =
        Option.map
            (fun i ->
                 assert (i < Array.length d.arr);
                 let k, v = d.arr.(i) in
                 assert (k = key);
                 v)
            (Map.find_opt key d.map)


    let add (key: Key.t) (value: 'a) (d: 'a t): 'a t =
        match Map.find_opt key d.map with
        | None ->
            let n = Array.length d.arr in
            {
                map = Map.add key n d.map;
                arr = Array.push (key, value) d.arr;
            }
        | Some _ ->
            d



    let set (key: Key.t) (f: 'a option -> 'a) (d: 'a t): 'a t =
        match Map.find_opt key d.map with
        | None ->
            add key (f None) d
        | Some idx ->
            let (k, v) = d.arr.(idx) in
            assert (k = key);
            {d with
             arr = Array.replace idx (key, f (Some v)) d.arr}



    let of_list (lst: (Key.t * 'a) list): 'a t =
        let rec of_list d = function
            | [] ->
                d
            | (key, value) :: tl ->
                of_list (add key value d) tl
        in
        of_list empty lst




    let fold (f: 'accu -> Key.t -> 'a -> 'accu) (accu: 'accu) (d: 'a t): 'accu =
        Array.fold_left
            (fun accu (key, value) -> f accu key value)
            accu
            d.arr


    let iter (f: Key.t -> 'a -> unit) (dict: 'a t): unit =
        Stdlib.Array.iter
            (fun (key, value) -> f key value)
            dict.arr


    let diff
            (add: Key.t -> 'a -> unit)
            (set: Key.t -> 'a -> unit)
            (remove: Key.t -> unit)
            (d1: 'a t)
            (d2: 'a t)
        : unit
        =
        iter
            (fun key _ ->
                 match find_opt key d1 with
                 | Some _ ->
                     ()
                 | None ->
                     remove key)
            d2;
        iter
            (fun key value ->
                 match find_opt key d2 with
                 | None ->
                     add key value

                 | Some value2 ->
                     if value == value2 then
                         ()
                     else
                         set key value)
            d1
end



include Make (String)
