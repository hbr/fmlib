module type ORDER =
sig
    val order: int
end


module O32: ORDER =
struct
    let order = 32
end




module Map0 (Order: ORDER) (Key: Interfaces.SORTABLE)  =
struct
    include Order

    let odd_order: bool =
        assert (3 <= order);
        order / 2 * 2 < order

    let max_keys: int = order - 1

    let min_keys: int =
        if odd_order then
            (order - 1) / 2
        else
            order / 2 - 1


    type key = Key.t

    type 'a pairs = (Key.t * 'a) array

    type 'a t =
        | Leaf of 'a pairs
        | Node of 'a pairs * 'a t array





    (* General functions. *)


    let is_empty (map: 'a t): bool =
        match map with
        | Leaf keys ->
            Array.length keys = 0
        | Node _ ->
            false


    let rec cardinal (map: 'a t): int =
        match map with
        | Leaf keys ->
            Array.length keys
        | Node (keys, children) ->
            Array.fold_left
                (fun n child -> n + cardinal child)
                (Array.length keys)
                children

    let empty: 'a t =
        Leaf [||]


    let fold_left (f: 'a -> Key.t -> 'b -> 'a) (start: 'a) (map: 'b t): 'a =
        let rec fold accu map =
            match map with
            | Leaf pairs ->
                Array.fold_left
                    (fun a (key,value) -> f a key value)
                    accu
                    pairs
            | Node (pairs, children) ->
                let n = Array.length pairs in
                assert (n + 1 = Array.length children);
                let rec fold_interior accu i =
                    assert (i < Array.length children);
                    if i = n then
                        fold accu children.(i)
                    else
                        fold_interior
                            (f
                                 (fold accu children.(i))
                                 (fst pairs.(i))
                                 (snd pairs.(i)))
                            (i + 1)
                in
                fold_interior accu 0
        in
        fold start map


    let fold_right (f: 'a -> Key.t -> 'b -> 'a) (start: 'a) (map: 'b t): 'a =
        let rec fold accu map =
            match map with
            | Leaf pairs ->
                Array.fold_right
                    (fun (key,value) a -> f a key value)
                    pairs
                    accu
            | Node (pairs, children) ->
                let n = Array.length pairs in
                assert (n + 1 = Array.length children);
                let rec fold_interior accu i =
                    assert (0 <= i);
                    if i = 0 then
                        accu
                    else
                        let i = i - 1 in
                        assert (0 <= i);
                        assert (i < Array.length children);
                        fold_interior
                            (fold
                                 (f accu (fst pairs.(i)) (snd pairs.(i)))
                                 children.(i))
                            i
                in
                fold_interior (fold accu children.(n)) n
        in
        fold start map



    let bindings (map: 'a t): (Key.t * 'a) list =
        fold_right
            (fun lst key value -> (key,value) :: lst)
            []
            map


    let keys (map: 'a t): Key.t list =
        fold_right
            (fun lst key _ -> key :: lst)
            []
            map








    (* Searching *)


    let bsearch (key: Key.t) (arr: 'a pairs): int * bool =
        Array.binsearch Key.compare fst key arr



    let rec find_opt (key: Key.t) (map: 'a t): 'a option =
        match map with
        | Leaf pairs ->
            let i, exact = bsearch key pairs in
            if exact then
                Some (snd pairs.(i))
            else
                None
        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                Some (snd pairs.(i))
            else
                find_opt key children.(i)












    (* Insertion
     * =========
     *)

    type 'a insert =
        | Normal_insert of 'a t
        | Split_insert of 'a t * (Key.t * 'a) * 'a t


    let subarray (arr: 'a array) (start: int) (beyond: int): 'a array =
        (* The subarray of [arr] starting at [start] and ending one before [beyond]. *)
        assert (0 <= start);
        assert (start <= beyond);
        assert (beyond <= Array.length arr);
        Array.sub arr start (beyond - start)


    let insert_subarray
            (arr: 'a array) (i: int) (x: 'a) (start: int) (beyond: int)
        : 'a array
        =
        (* The subarray of [arr] starting at [start] and ending one before [beyond]
           with [x] inserted at position [i]. *)
        assert (0 <= start);
        assert (start <= i);
        assert (i <= beyond);
        assert (beyond <= Array.length arr);
        let arr2 = Array.make (beyond - start + 1) x in
        Array.blit arr start arr2 0 (i - start);
        Array.blit arr i arr2 (i - start + 1) (beyond - i);
        arr2



    let split_subarray
            (arr: 'a array) (i: int) (x: 'a) (y: 'a) (start: int) (beyond: int)
        : 'a array
        =
        (* The subarray of [arr] starting at [start] and ending one before [beyond]
           with [x] inserted at position [i] and the original value at position
           [i] replaced by [y]. *)
        assert (i < beyond);
        let arr = insert_subarray arr i x start beyond in
        arr.(i - start + 1) <- y;
        arr




    let add_in_leaf
            (key: Key.t) (value: 'a) (pairs: 'a pairs) (node: 'a t)
        : 'a insert
        =
        let len = Array.length pairs in
        let i, exact = bsearch key pairs in
        if exact then

            if value == snd pairs.(i) then
                Normal_insert node
            else
                Normal_insert (Leaf (Array.replace i (key, value) pairs))

        else if len < max_keys then
            (* Leaf is not full. *)
            Normal_insert (Leaf (Array.insert i (key, value) pairs))

        else
            (* Leaf is full *)
            let insert_subarray = insert_subarray pairs i (key, value)
            and k = order / 2
            in
            if odd_order then
                if i = k then
                    let left  = subarray pairs 0 k
                    and right = subarray pairs k len
                    in
                    Split_insert (Leaf left, (key, value), Leaf right)
                else if i < k then
                    let left  = insert_subarray 0 (k - 1)
                    and right = subarray pairs k len
                    in
                    Split_insert (Leaf left, pairs.(k - 1), Leaf right)
                else
                    let left  = subarray pairs 0 k
                    and right = insert_subarray (k + 1) len
                    in
                    Split_insert (Leaf left, pairs.(k), Leaf right)
            else begin
                (* even order *)
                if i < k then
                    let left  = insert_subarray 0 (k - 1)
                    and right = subarray pairs k len
                    in
                    Split_insert (Leaf left, pairs.(k - 1), Leaf right)
                else
                    let left  = subarray pairs 0 (k - 1)
                    and right = insert_subarray k len
                    in
                    Split_insert (Leaf left, pairs.(k - 1), Leaf right)
            end




    let add_in_node
            (i: int)
            (left: 'a t)
            (pair: Key.t * 'a)
            (right: 'a t)
            (pairs: 'a pairs)
            (children: 'a t array)
        : 'a insert
        =
        let len = Array.length pairs in
        if len < max_keys then
            let pairs = Array.insert i pair pairs
            and children = Array.insert i left children
            in
            assert (Array.valid_index (i + 1) children);
            children.(i + 1) <- right;
            Normal_insert (Node (pairs, children))
        else
            (* Node is full. *)
            let k = order / 2
            and insert_subarray = insert_subarray pairs i pair
            and split_subarray start beyond =
                split_subarray children i left right start beyond
            in
            if odd_order then
                if i = k then
                    let left_pairs     = subarray pairs    0 k
                    and left_children  = subarray children 0 (k + 1)
                    and right_pairs    = subarray pairs    k len
                    and right_children = subarray children k (len + 1)
                    in
                    assert (Array.valid_index k left_children);
                    assert (Array.valid_index 0 right_children);
                    left_children.(k)  <- left;
                    right_children.(0) <- right;
                    Split_insert (
                        Node (left_pairs, left_children),
                        pair,
                        Node (right_pairs, right_children))
                else if i < k then
                    let left_pairs     = insert_subarray 0 (k - 1)
                    and left_children  = split_subarray  0 k
                    and right_pairs    = subarray pairs    k len
                    and right_children = subarray children k (len + 1)
                    in
                    assert (Array.valid_index (k - 1) pairs);
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k - 1),
                        Node (right_pairs, right_children))
                else begin
                    let left_pairs     = subarray pairs    0 k
                    and left_children  = subarray children 0 (k + 1)
                    and right_pairs    = insert_subarray (k + 1) len
                    and right_children = split_subarray  (k + 1) (len + 1) in
                    assert (Array.valid_index k pairs);
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k),
                        Node (right_pairs, right_children))
                end
            else begin
                (* even order *)
                if i < k then
                    let left_pairs     = insert_subarray 0 (k - 1)
                    and left_children  = split_subarray  0 k
                    and right_pairs    = subarray pairs    k len
                    and right_children = subarray children k (len + 1)
                    in
                    assert (Array.valid_index (k - 1) pairs);
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k - 1),
                        Node (right_pairs, right_children))
                else
                    let left_pairs     = subarray pairs    0 (k - 1)
                    and left_children  = subarray children 0 k
                    and right_pairs    = insert_subarray k len
                    and right_children = split_subarray  k (len + 1)
                    in
                    assert (Array.valid_index (k - 1) pairs);
                    Split_insert (
                        Node (left_pairs, left_children),
                        pairs.(k - 1),
                        Node (right_pairs, right_children))
            end



    let rec add_aux (key: Key.t) (value: 'a) (map: 'a t): 'a insert =
        match map with
        | Leaf pairs as node ->
            add_in_leaf key value pairs node

        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                (* An exact match has been found. Therefore update the value. *)
                let pairs = Array.replace i (key,value) pairs in
                Normal_insert (Node (pairs, children))
            else begin
                (** Add the key value pair into the [i]th child. *)
                assert (Array.valid_index i children);
                match add_aux key value children.(i) with
                | Normal_insert child ->
                    let children = Array.replace i child children in
                    Normal_insert (Node (pairs, children))
                | Split_insert (u, y, v) ->
                    add_in_node i u y v pairs children
            end



    let add (key: Key.t) (value: 'a) (map: 'a t): 'a t =
        match add_aux key value map with
        | Normal_insert map ->
            map
        | Split_insert (left, pair, right) ->
            (* tree grows at the root *)
            Node ([|pair|], [|left; right|])


    let of_list (lst: (Key.t * 'a) list):  'a t =
        List.fold_left
            (fun map (k, v) -> add k v map)
            empty
            lst




    (* Deletion
     * ========
     *)

    type 'a delete = {
        tree:  'a t;        (* The tree with the deleted key value pair. *)
        pair:   Key.t * 'a; (* The deleted key value pair. *)
        underflow: bool;    (* one key less than the minimal number *)
    }


    let not_minimal (pairs: 'a pairs): bool =
        min_keys < Array.length pairs


    let replace2
            (i: int) (left: 'a t) (right: 'a t) (children: 'a t array)
        : 'a t array
        =
        let children = Array.copy children in
        assert (Array.valid_index i children);
        assert (Array.valid_index (i + 1) children);
        children.(i)     <- left;
        children.(i + 1) <- right;
        children


    let rotate_keys
            (to_left: bool)
            (i: int) (left: 'a pairs) (parent: 'a pairs) (right: 'a pairs)
        : 'a pairs * 'a pairs * 'a pairs
        =
        let open Array in
        assert (valid_index i parent);
        if to_left then
            push parent.(i) left,
            replace i (first right) parent,
            remove_first right
        else
            remove_last left,
            replace i (last left) parent,
            push_front parent.(i) right


    let rotate_children
            (to_left: bool)
            (left: 'a t array) (right: 'a t array)
        : 'a t array * 'a t array
        =
        let open Array in
        if to_left then
            push (first right) left,
            remove_first right
        else
            remove_last left,
            push_front (last left) right



    let merge_keys
            (i: int) (left: 'a pairs) (parent: 'a pairs) (right: 'a pairs)
        : 'a pairs * 'a pairs
        =
        assert (Array.valid_index i parent);
        let len_left  = Array.length left
        and len_right = Array.length right
        in
        let merged = Array.make (len_left + 1 + len_right) parent.(i)
        and parent = Array.remove i parent
        in
        Array.blit left  0 merged 0 len_left;
        Array.blit right 0 merged (len_left + 1) len_right;
        merged, parent


    let merge_leaves
            (i: int)
            (pair: Key.t * 'a)
            (pairs1: 'a pairs) (pairs2: 'a pairs)
            (pairs: 'a pairs) (children: 'a t array)
        : 'a delete
        =
        assert (i + 1 < Array.length children);
        let merged, pairs = merge_keys i pairs1 pairs pairs2
        and children      = Array.remove i children
        and underflow     = Array.length pairs <= min_keys
        in
        children.(i) <- Leaf merged;
        {tree = Node (pairs, children); pair; underflow}



    let merge_nodes
            (i: int)
            (pair: Key.t * 'a)
            (pairs1: 'a pairs) (children1: 'a t array)
            (pairs2: 'a pairs) (children2: 'a t array)
            (pairs: 'a pairs) (children: 'a t array)
        : 'a delete
        =
        assert (i + 1 < Array.length children);
        let pairs_new, pairs = merge_keys i pairs1 pairs pairs2
        and children      = Array.remove i children
        and underflow     = Array.length pairs <= min_keys
        and children_new  = Array.append children1 children2
        in
        children.(i) <- Node (pairs_new, children_new);
        {tree = Node (pairs, children); pair; underflow}



    let handle_underflow
            (i: int)                (* Index of the child where the deletion occurred. *)
            (underflow_left: bool)  (* Underflow happend in the left child? *)
            (left_child: 'a t)
            (right_child: 'a t)
            (pair: Key.t * 'a)      (* The deleted key value pair. *)
            (pairs: 'a pairs)       (* The key value pairs of the parent. *)
            (children: 'a t array)  (* The children of the parent. *)
        : 'a delete
        =
        let not_minimal pairs1 pairs2 =
            if underflow_left then
                not_minimal pairs2
            else
                not_minimal pairs1
        in
        match left_child, right_child with
        | Leaf pairs1, Leaf pairs2 when not_minimal pairs1 pairs2 ->
            (* Right sibling is not minimal, rotate *)
            let pairs1, pairs, pairs2 =
                rotate_keys underflow_left i pairs1 pairs pairs2
            in
            let children =
                replace2 i (Leaf pairs1) (Leaf pairs2) children
            in
            {tree = Node (pairs, children); pair; underflow = false}

        | Leaf pairs1, Leaf pairs2 ->
            (* Sibling is minimal, merge *)
            merge_leaves i pair pairs1 pairs2 pairs children

        | Node (pairs1, children1), Node (pairs2, children2)
            when not_minimal pairs1 pairs2
            ->
            (* Sibling is not minimal, rotate *)
            let pairs1, pairs, pairs2 =
                rotate_keys underflow_left i pairs1 pairs pairs2
            and children1, children2 =
                rotate_children underflow_left children1 children2
            in
            let children =
                replace2
                    i
                    (Node (pairs1, children1))
                    (Node (pairs2, children2))
                    children
            in
            {tree = Node (pairs, children); pair; underflow = false}

        | Node (pairs1, children1), Node (pairs2, children2) ->
            (* Sibling is minimal, merge *)
            merge_nodes
                i pair
                pairs1 children1
                pairs2 children2
                pairs children

        | _, _ ->
            assert false (* Cannot happen, tree is balanced. *)



    let handle_delete
            (i: int)                (* Index of the child where the deletion occurred. *)
            (pair: Key.t * 'a)      (* The deleted key value pair. *)
            (d: 'a delete)          (* The new tree with the key value pair deleted. *)
            (pairs: 'a pairs)       (* The key value pairs of the parent. *)
            (children: 'a t array)  (* The children of the parent. *)
        : 'a delete
        =
        if not d.underflow then
            {
                tree = Node (pairs, Array.replace i d.tree children);
                pair;
                underflow = false
            }
        else
            let len = Array.length pairs in
            if i < len then
                handle_underflow i true d.tree children.(i + 1) pair pairs children
            else
                let i = i - 1 in
                handle_underflow i false children.(i) d.tree pair pairs children



    let rec remove_last (map: 'a t): 'a delete =
        match map with
        | Leaf pairs ->
            let len = Array.length pairs in
            assert (0 < len);
            let pair  = Array.last pairs
            and pairs = Array.remove_last pairs
            and underflow = Array.length pairs <= min_keys
            in
            {
                tree = Leaf pairs;
                pair;
                underflow
            }
        | Node (pairs, children) ->
            let len = Array.length pairs in
            assert (len + 1 = Array.length children);
            let d = remove_last children.(len) in
            handle_delete len d.pair d pairs children




    let rec remove_aux (key: Key.t) (map: 'a t): 'a delete option =
        match map with
        | Leaf pairs ->
            let i, exact = bsearch key pairs in
            if exact then
                let pair =  pairs.(i)
                and pairs = Array.remove i pairs
                and underflow = Array.length pairs <= min_keys
                in
                Some {
                    tree = Leaf pairs;
                    pair;
                    underflow
                }
            else
                None

        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                let d = remove_last children.(i) in
                let pair  = pairs.(i)
                and pairs = Array.replace i d.pair pairs in
                Some (handle_delete i pair d pairs children)
            else
                Option.map
                    (fun d -> handle_delete i d.pair d pairs children)
                    (remove_aux key children.(i))



    let remove (key: Key.t) (map: 'a t): 'a t =
        match remove_aux key map with
        | None ->
            map
        | Some d ->
            match d.tree with
            | Node (pairs, children) when Array.is_empty pairs ->
                (* tree shrinks at the root *)
                children.(0)
            | _ ->
                d.tree




    (* Update
     * ======
     *)

    type 'a update =
        | Insert of 'a insert
        | Delete of 'a delete


    let rec update_aux
            (key: Key.t) (f: 'a option -> 'a option) (map: 'a t)
        : 'a update
        =
        match map with
        | Leaf pairs as node ->
            let i, exact = bsearch key pairs in
            if exact then
                let v_old = snd pairs.(i)
                in
                match f (Some v_old) with
                | None ->
                    let pairs = Array.remove i pairs
                    and pair  = pairs.(i)
                    and underflow = min_keys = Array.length pairs in
                    Delete {
                        tree = Leaf pairs;
                        pair;
                        underflow}
                | Some value ->
                    if value == v_old then
                        Insert (Normal_insert node)
                    else
                        Insert (
                            Normal_insert
                                (Leaf (Array.replace i (key,value) pairs))
                        )
            else begin
                match f None with
                | None ->
                    Insert (Normal_insert map)
                | Some value ->
                    Insert (add_in_leaf key value pairs node)
            end

        | Node (pairs, children) ->
            let i, exact = bsearch key pairs in
            if exact then
                match f (Some (snd pairs.(i))) with
                | None ->
                    let d = remove_last children.(i) in
                    let pair  = pairs.(i)
                    and pairs = Array.replace i d.pair pairs in
                    Delete (handle_delete i pair d pairs children)

                | Some value ->
                    Insert (Normal_insert (Node (
                        Array.replace i (key, value) pairs,
                        children
                    )))
            else
                match update_aux key f children.(i) with
                | Insert (Normal_insert child) ->
                    Insert (Normal_insert (Node (
                        pairs,
                        Array.replace i child children
                    )))

                | Insert (Split_insert (u, y, v)) ->
                    Insert (add_in_node i u y v pairs children)

                | Delete d ->
                    Delete (handle_delete i d.pair d pairs children)




    let update (key: Key.t) (f: 'a option -> 'a option) (map: 'a t): 'a t =
        match update_aux key f map with
        | Insert (Normal_insert map) ->
            map

        | Insert (Split_insert (u, y, v)) ->
            Node ( [| y |], [| u; v |] )

        | Delete d ->
            match d.tree with
            | Node (pairs, children) when Array.length pairs = 0 ->
                (* tree shrinks at the root *)
                children.(0)
            | _ ->
                d.tree








    (* Stream of key value pairs
     * =========================
     *)

    type 'a entry
        =
        'a pairs
        * 'a t array
        * int

    type 'a source = {
        top:
            'a t * int;         (* node/leaf and position within the node/leaf *)

        stack: 'a entry list;
    }


    let has_more (source: 'a source): bool =
        match source.top with
        | Leaf pairs, i ->
            i < Array.length pairs
        | Node (pairs, _ ), i ->
            i < Array.length pairs


    let peek (source: 'a source): Key.t * 'a =
        assert (has_more source);
        match source.top with
        | Leaf pairs, i ->
            pairs.(i)
        | Node (pairs, _ ), i ->
            pairs.(i)



    let rec down (tree: 'a t) (stack: 'a entry list): 'a source =
        (* Search for the first key value pair of [tree]. *)
        match tree with
        | Leaf pairs ->
            (* We are already on a leaf. The next item is the first key value
             * pair. *)
            {top = Leaf pairs, 0; stack}

        | Node (pairs, children) ->
            (* Search the first key value pair in the first child. Push the
             * first key value pair of the node onto the stack. *)
            down children.(0) ((pairs, children, 0) :: stack)



    let rec up (stack: 'a entry list): 'a source =
        (* Search the stack for a node which is positioned on a key value pair.
         * *)
        match stack with
        | [] ->
            {top = empty, 0; stack = []}
        | (pairs, children, i) :: stack ->
            if i < Array.length pairs then
                {top = Node (pairs, children), i; stack}
            else
                up stack


    let advance (source: 'a source): 'a source =
        assert (has_more source);
        match source.top with
        | Leaf pairs, i ->
            if i + 1 < Array.length pairs then
                {source with top = Leaf pairs, i + 1}
            else
                up source.stack
        | Node (pairs, children), i ->
            assert (i < Array.length pairs);
            down
                children.(i + 1)
                ((pairs, children, i + 1) :: source.stack)


    let make_source (tree:  'a t): 'a source =
        down tree []






    module Source (Value: Interfaces.ANY) = struct
        type 'a map = 'a t
        type item   = Key.t * Value.t

        type t = Value.t source

        let has_more = has_more
        let peek     = peek
        let advance  = advance
        let make     = make_source
    end
end








module Set0 (Order: ORDER) (Key: Interfaces.SORTABLE) = struct
    module Map = Map0 (Order) (Key)

    type item = Key.t

    type t = unit Map.t


    let is_empty = Map.is_empty


    let cardinal = Map.cardinal


    let empty = Map.empty


    let fold_left (f: 'a -> Key.t -> 'a) (start: 'a) (set: t): 'a =
        Map.fold_left
            (fun a key _ -> f a key)
            start
            set


    let fold_right (f: 'a -> Key.t -> 'a) (start: 'a) (set: t): 'a =
        Map.fold_right
            (fun a key _ -> f a key)
            start
            set


    let mem (key: Key.t) (set: t): bool =
        match Map.find_opt key set with
        | None ->
            false
        | Some _ ->
            true


    let add (key: Key.t) (set: t): t =
        Map.add key () set


    let remove (key: Key.t) (set: t): t =
        Map.remove key set


    let elements (set: t): Key.t list =
        Map.keys set


    let of_list (lst: Key.t list): t =
        List.fold_left
            (fun set k -> add k set)
            empty
            lst



    module Source = struct
        type set = t

        module M =  Map.Source (Unit)

        type item = Key.t
        type t = M.t

        let has_more = M.has_more

        let peek (source: t): Key.t = M.peek source |> fst

        let advance = M.advance

        let make = M.make
    end
end









module Map (Key: Interfaces.SORTABLE) =
struct
    include Map0 (O32) (Key)
end








module Set (Key: Interfaces.SORTABLE) =
struct
    include Set0 (O32) (Key)
end








(* ================================================================
 * Unit Tests
 *
 * with sets
 * ================================================================
 *)

module Set_order (Order: ORDER) = struct
    include Set0 (Order) (Int)

    let do_upward (f: int -> t -> t) (start: int) (beyond: int) (set: t): t =
        assert (start <= beyond);
        let rec action i set =
            if i = beyond then
                set
            else
                action (i + 1) (f i set)
        in
        action start set

    let do_downward (f: int -> t -> t) (start: int) (beyond: int) (set: t): t =
        assert (start <= beyond);
        let rec action i set =
            if i = start then
                set
            else
                let i = i - 1 in
                action i (f i set)
        in
        action beyond set


    let add_upward (start: int) (beyond: int) (set: t): t =
        do_upward add start beyond set


    let add_downward (start: int) (beyond: int) (set: t): t =
        do_downward add start beyond set

    let remove_upward (start: int) (beyond: int) (set: t): t =
        do_upward remove start beyond set

    let remove_downward (start: int) (beyond: int) (set: t): t =
        do_downward remove start beyond set

    let check_range (start: int) (beyond: int) (set: t): bool =
        let n, ok =
            fold_left
                (fun (i, ok) key -> i + 1, ok && key = i)
                (start, true)
                set
        in
        n = beyond && ok

    module Source = struct
        include Source

        let to_list (source: t): int list =
            let rec to_list source accu =
                if has_more source then
                    to_list (advance source) (peek source :: accu)
                else
                    List.rev accu
            in
            to_list source []
    end
end


module Set3 = Set_order (struct let order = 3 end)
module Set4 = Set_order (struct let order = 4 end)

let string_of (lst: int list): string =
    "["
    ^
    String.concat
        ", "
        (List.map string_of_int lst)
    ^
    "]"
let _ = string_of


let%test _ =
    let module Map = Map (Int) in
    Map.(cardinal empty) = 0


(* Insertion *)

let%test _ =
    let set = Set4.(add_upward 100 200 empty) in
    Set4.check_range 100 200 set




let%test _ =
    let set = Set4.(add_downward 0 100 empty) in
    Set4.check_range 0 100 set




let%test _ =
    let set = Set3.(add_upward 100 200 empty) in
    Set3.check_range 100 200 set




let%test _ =
    let set = Set3.(add_downward 0 100 empty) in
    Set3.check_range 0 100 set





(* Deletion *)


let%test _ =
    let set = Set3.(add_upward 0 200 empty |> remove_upward 0 100) in
    Set3.check_range 100 200 set


let%test _ =
    let set = Set3.(add_upward 0 200 empty |> remove_downward 0 100) in
    Set3.check_range 100 200 set


let%test _ =
    let set = Set4.(add_upward 0 200 empty |> remove_upward 0 100) in
    Set4.check_range 100 200 set


let%test _ =
    let set = Set4.(add_upward 0 200 empty |> remove_downward 0 100) in
    Set4.check_range 100 200 set







(* ================================================================
 * Unit Tests
 *
 * with maps
 * ================================================================
 *)

module Map_order (Order: ORDER) = struct
    include Map0 (Order) (Int)

    let do_upward
            (f: int -> 'a t -> 'a t) (start: int) (beyond: int) (map: 'a t)
        : 'a t
        =
        assert (start <= beyond);
        let rec action i map =
            if i = beyond then
                map
            else
                action (i + 1) (f i map)
        in
        action start map

    let do_downward
            (f: int -> 'a t -> 'a t) (start: int) (beyond: int) (map: 'a t)
        : 'a t
        =
        assert (start <= beyond);
        let rec action i map =
            if i = start then
                map
            else
                let i = i - 1 in
                action i (f i map)
        in
        action beyond map


    let add_upward (start: int) (beyond: int) (f: int -> 'a) (map: 'a t): 'a t =
        do_upward (fun i map -> add i (f i) map) start beyond map


    let add_downward (start: int) (beyond: int) (f: int -> 'a) (map: 'a t): 'a t =
        do_downward (fun i map -> add i (f i) map) start beyond map


    let update_upward
            (start: int) (beyond: int) (f: int -> 'a option -> 'a option) (map: 'a t)
        : 'a t
        =
        do_upward (fun i map -> update i (f i) map) start beyond map


    let check_range (start: int) (beyond: int) (f: int -> 'a) (map: 'a t): bool =
        let n, ok =
            fold_left
                (fun (i, ok) key value -> i + 1, ok && f key = value)
                (start, true)
                map
        in
        n = beyond && ok
end


module Map3 = Map_order (struct let order = 3 end)


(* Insertion *)

let%test _ =
    let open Map3 in
    let map = add_upward 0 100 Fun.id empty in
    check_range 0 100 Fun.id map

let%test _ =
    let open Map3 in
    let map = add_downward 0 100 Fun.id empty in
    check_range 0 100 Fun.id map



(* Update *)

let%test _ =
    let open Map3 in
    let map = update_upward 0 100 (fun i _ -> Some i) empty in
    check_range 0 100 Fun.id map


let%test _ =
    let open Map3 in
    let map =
        add_upward 0 100 Fun.id empty
        |>
        update_upward 0 100 (fun _ -> Option.map (fun i -> 2 * i))
    in
    check_range 0 100 (fun i -> 2 * i) map


let%test _ =
    let open Map3 in
    let map =
        add_upward 0 100 Fun.id empty
        |>
        update_upward 0 100 (fun _ _ -> None)
    in
    is_empty map



let%test _ =
    let open Map3 in
    let f i = if i / 2 * 2 = i then i else 2 * i
    in
    let map =
        add_upward 0 100 Fun.id empty
        |>
        add_upward 200 300 Fun.id
        |>
        update_upward 0 100 (fun _ -> Option.map f)
        |>
        update_upward 100 200 (fun i _ -> Some (f i))
        |>
        update_upward 0 300
            (fun i ->
                 if i < 200 then
                     Option.map Fun.id
                 else
                     fun _ -> None)
    in
    check_range 0 200 f map






(* ================================================================
 * Unit Tests
 *
 * with streams
 * ================================================================
 *)

let%test _ =
    let open Set3 in
    let module Source = Set3.Source in
    let set = add_upward 0 20 empty in
    Source.(make set |> to_list) = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12;
                                    13; 14; 15; 16; 17; 18; 19]
