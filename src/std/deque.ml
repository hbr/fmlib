module Basic =
struct
    type 'a t = {
        front: 'a list;
        rear:  'a list;
    }


    let empty: 'a t =
        {front = []; rear = [];}


    let push_front (e: 'a) (fifo: 'a t): 'a t =
        {fifo with front = e :: fifo.front}


    let push_rear (e: 'a) (fifo: 'a t): 'a t =
        {fifo with rear = e :: fifo.rear}


    let pop_front (fifo: 'a t): ('a * 'a t) option =
        match fifo.front with
        | hd :: front ->
            Some (hd, {fifo with front})

        | [] ->
            match List.rev fifo.rear with
            | [] ->
                None

            | hd :: front ->
                Some (hd, {front; rear = []})

    let prepend (fifo: 'a t) (lst: 'a list): 'a list =
        fifo.front @ List.rev_append fifo.rear lst
end




type 'a t =
    | Empty
    | Nonempty of 'a Basic.t * 'a


let is_empty (f: 'a t): bool =
    match f with
    | Empty ->
        true
    | _ ->
        false


let has_some (f: 'a t): bool =
    not (is_empty f)


let empty: 'a t =
    Empty


let push_front (e: 'a) (f: 'a t): 'a t =
    match f with
    | Empty ->
        Nonempty (Basic.empty, e)
    | Nonempty (lf, last) ->
        Nonempty (Basic.push_front e lf, last)


let push_rear (e: 'a) (f: 'a t): 'a t =
    match f with
    | Empty ->
        Nonempty (Basic.empty, e)
    | Nonempty (lf, last) ->
        Nonempty (Basic.push_rear last lf, e)


let pop_front (f: 'a t): ('a * 'a t) option =
    match f with
    | Empty ->
        None
    | Nonempty (lf, last) ->
        match Basic.pop_front lf with
        | None ->
            Some (last, empty)
        | Some (first, lf) ->
            Some (first, Nonempty (lf, last))


let update_first (f: 'a -> 'a) (fifo: 'a t): 'a t =
    match fifo with
    | Empty ->
        Empty
    | Nonempty (lf, last) ->
        match Basic.pop_front lf with
        | None ->
            Nonempty (lf, f last)
        | Some (first, lf) ->
            Nonempty (Basic.push_front (f first) lf, last)


let update_last (f: 'a -> 'a) (fifo: 'a t): 'a t =
    match fifo with
    | Empty ->
        Empty
    | Nonempty (lf, last) ->
        Nonempty (lf, f last)



let to_list (fifo: 'a t): 'a list =
    match fifo with
    | Empty ->
        []
    | Nonempty (lf, last) ->
        Basic.prepend lf [last]










(* ---------------------------------------------------------- *)
(* Unit Tests *)
(* ---------------------------------------------------------- *)

let%test _ =
    to_list
        (empty |> push_rear 0 |> push_rear 1 |> push_rear 2)
    =
    [0; 1; 2]


let%test _ =
    empty |> push_rear 0 |> pop_front
    =
    Some (0, empty)


let%test _ =
    match
        empty
        |> push_rear 0 |> push_rear 1
        |> update_first (fun _ -> 10) |> pop_front
    with
    | Some (10, fifo ) ->
        pop_front fifo = Some (1, empty)
    | _ ->
        false


let%test _ =
    match
        empty
        |> push_rear 0 |> push_rear 1
        |> update_last (fun _ -> 10) |> pop_front
    with
    | Some (0, fifo ) ->
        pop_front fifo = Some (10, empty)
    | _ ->
        false
