module type BITSIZE =
sig
    val bitsize: int
end




module Make (Bitsize: BITSIZE) =
struct
    include Bitsize

    let branching: int =
        assert (0 < bitsize);
        1 lsl bitsize


    let slot (i: int) (l: int): int =
        (* The slot of index [i] at level [l].

            i/ 2^(l * bitsize)
         *)
        i lsr (l * bitsize)


    let offset (i: int) (s: int) (l: int): int =
        (* The offset of index [i] in slot [s] in a tree at level [l].

            i- s * 2^(l * bitsize)
         *)
        i - s lsl (l * bitsize)


    let full_size (l: int): int =
        (* The size of a full radix balanced array at level [l]. *)
        assert (0 <= l);
        1 lsl ((l + 1) * bitsize)


    type 'a t =
        | Leaf of
            'a array
        | Node of {
            size:  int;
            level: int;
            nodes: 'a t array}


    let level: 'a t -> int = function
        | Leaf _ ->
            0
        | Node node ->
            node.level


    let is_full: 'a t -> bool = function
        | Leaf arr ->
            Array.length arr = full_size 0
        | Node node ->
            node.size = full_size node.level


    let length: 'a t -> int =
        (* The length of the radix balanced array. *)
        function
        | Leaf arr ->
            Array.length arr
        | Node node ->
            node.size


    let has_some (t: 'a t): bool =
        0 < length t


    let is_empty (t: 'a t): bool =
        0 = length t



    let check_invariant (t: 'a t):  bool =
        let rec check is_root = function
            | Leaf arr ->
                let len = Array.length arr in
                len <= branching
                &&
                (is_root || 0 < len)
            | Node node ->
                let nchildren = Array.length node.nodes
                in
                Array.for_all (check false) node.nodes
                &&
                Array.for_all
                    (fun child -> level child + 1 = node.level)
                    node.nodes
                &&
                nchildren <= branching
                &&
                1 <= nchildren
                &&
                (not is_root || 2 <= nchildren)
                &&
                (
                    node.size
                    =
                    Array.fold_left
                        (fun size child -> size + length child)
                        0
                        node.nodes
                )
        in
        check true t


    let empty: 'a t =
        Leaf [| |]



    (* Folding
     *)

    let fold_left (f: 'a -> 'b -> 'a) (start: 'a) (t: 'b t): 'a =
        let rec fold start = function
            | Leaf arr ->
                Array.fold_left f start arr
            | Node node ->
                Array.fold_left fold start node.nodes
        in
        fold start t



    let foldi_left (f: 'a -> int -> 'b -> 'a) (start: 'a) (t: 'b t): 'a =
        fold_left
            (fun (start,idx) e -> f start idx e, (idx + 1))
            (start, 0)
            t
        |>
        fst



    (* Element Retrieval
     *)

    let rec element (i: int) (t: 'a t): 'a =
        (* The element at index [i] in the radix balanced array [t]. *)
        assert (0 <= i);
        assert (i < length t);
        match t with
        | Leaf arr ->
            arr.(i)
        | Node node ->
            let s = slot i node.level in
            let o = offset i s node.level in
            element o node.nodes.(s)


    let first (t: 'a t): 'a =
        (* The first element of the non empty radix balanced array [t]. *)
        assert (has_some t);
        let rec fst = function
            | Leaf arr ->
                Array.first arr
            | Node node ->
                fst (Array.first node.nodes)
        in
        fst t


    let last (t: 'a t): 'a =
        (* The last element of the non empty radix balanced array [t]. *)
        assert (has_some t);
        let rec fst = function
            | Leaf arr ->
                Array.last arr
            | Node node ->
                fst (Array.last node.nodes)
        in
        fst t



    (* Element Replacement
     *)


    let rec replace (i: int) (e: 'a) (t: 'a t): 'a t =
        (* Replace the element at index [i] by the element [e] within the radix
           balanced array [t]. *)
        assert (0 <= i);
        assert (i < length t);
        match t with
        | Leaf arr ->
            Leaf (Array.replace i e arr)
        | Node node ->
            let s = slot i node.level in
            let o = offset i s node.level in
            Node
                {node with
                    nodes =
                        Array.replace
                            s
                            (replace o e node.nodes.(s))
                            node.nodes
                }






    (* Element Insertion at the Rear End
     *)

    let rec singleton_tree (lev: int) (e: 'a): 'a t =
        (* Construct tree at level [lev] with the element [e]. *)
        if lev = 0 then
            Leaf [| e |]
        else
            Node {
                size = 1;
                level = lev;
                nodes = [| singleton_tree (lev - 1) e |]
            }


    let rec push_not_full (e: 'a) (t: 'a t): 'a t =
        (* Append the element [e] at the rear end of the radix balanced array
           [t] which is not full. *)
        assert (not (is_full t));
        match t with
        | Leaf arr ->
            Leaf (Array.push e arr)

        | Node node ->
            let slot = Array.length node.nodes - 1 in
            assert (0 <= slot);
            let nodes =
                if is_full node.nodes.(slot) then
                    Array.push
                        (singleton_tree (node.level - 1) e)
                        node.nodes
                else
                    Array.replace
                        slot
                        (push_not_full e node.nodes.(slot))
                        node.nodes
            in
            Node
                {node with nodes; size = node.size + 1}


    let push (e: 'a) (t: 'a t): 'a t =
        (* Append the element [e] at the rear end of the radix balanced array
           [t]. *)
        let lev = level t
        and len = length t
        in
        if len = full_size lev then
            Node {
                size  = len + 1;
                level = lev + 1;
                nodes = [| t; singleton_tree lev e|]
            }
        else
            push_not_full e t







    (* Element Removal from the Rear End
     *)


    let rec pop_aux (is_root: bool) (t: 'a t): 'a * 'a t =
        (* Remove the last element from a nonempty tree. *)
        assert (has_some t);
        match t with
        | Leaf arr ->
            Array.(last arr, Leaf (remove_last arr))
        | Node node ->
            let j = Array.length node.nodes - 1 in
            assert (0 <= j);
            let child = node.nodes.(j) in
            let len   = length child in
            if is_root && j = 1 && len = 1 then
                (* Last child of the root node has only one element. *)
                last child,
                node.nodes.(0)
            else
                let e, nodes =
                    if len = 1 then
                        (* Last child has only one element. *)
                        last child,
                        Array.remove_last node.nodes
                    else
                        (* Normal case. *)
                        let e, child = pop_aux false child in
                        e,
                        Array.replace j child node.nodes
                in
                e,
                Node {
                    node with
                    size = node.size - 1;
                    nodes
                }


    let pop (t: 'a t): 'a * 'a t =
        assert (has_some t);
        pop_aux true t


    let pop_opt (t: 'a t): ('a * 'a t) option =
        if is_empty t then
            None
        else
            Some (pop_aux true t)
end




module Branching2: BITSIZE =
struct
    let bitsize: int = 1
end


module Branching32: BITSIZE =
struct
    let bitsize: int = 5
end



include Make (Branching32)







(* Unit Tests
 * **********
 *)
module Rb = Make (Branching2)


let fill (start: int) (beyond: int): int Rb.t =
    assert (start <= beyond);
    let rec fl start t =
        if start = beyond then
            t
        else
            fl (start + 1) (Rb.push start t)
    in
    fl start Rb.empty


let check_fill (start: int) (beyond: int): bool =
    let rec check start t =
        if start = beyond then
            Rb.check_invariant t
        else
            Rb.check_invariant t
            &&
            check (start + 1) (Rb.push start t)
    in
    check start Rb.empty


let check_fold (start: int) (beyond: int) (t: int Rb.t): bool =
    start + Rb.length t = beyond
    &&
    Rb.foldi_left
        (fun ok idx e ->
             ok && e = start + idx)
        true
        t


let check_element (start: int) (beyond: int) (t: int Rb.t): bool =
    let rec check_from i start =
        if start = beyond then
            true
        else
            start = Rb.element i t
            &&
            check_from (i + 1) (start + 1)
    in
    check_from 0 start



let check_pop (start: int) (beyond: int) (t: int Rb.t): bool =
    let rec check beyond t =
        Rb.check_invariant t
        &&
        (
            if beyond = start then
                Rb.is_empty t
            else
                Rb.has_some t
                &&
                let e, t = Rb.pop t in
                e + 1 = beyond
                &&
                check (beyond - 1) t
        )
    in
    check beyond t



let%test _ =
    let start = 10
    and beyond = 100
    in
    check_fill start beyond


let%test _ =
    let start = 10
    and beyond = 100
    in
    check_fold start beyond (fill start beyond)


let%test _ =
    let start = 10
    and beyond = 100
    in
    check_pop start beyond (fill start beyond)



let%test _ =
    let start = 10
    and beyond = 100
    in
    check_element start beyond (fill start beyond)



let%test _ =
    Rb.(check_invariant empty)


let%test _ =
    Rb.check_invariant (fill 0 25)
