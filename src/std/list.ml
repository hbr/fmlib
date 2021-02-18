include Stdlib.List
include Interfaces

type 'a t = 'a list

let return (a:'a): 'a t = [a]

let rec (>>=) (l:'a t) (f:'a -> 'b t): 'b t =
  match l with
  | [] ->
     []
  | hd :: tl ->
     f hd @ (tl >>= f)

let ( let* ) = (>>=)


let (>=>) (f:'a -> 'b t) (g:'b -> 'c t) (a:'a): 'c t =
  f a >>= g


let (<*>) (flst: ('a -> 'b) t) (lst:'a t): 'b t =
  flst >>= fun f -> map f lst

let join = concat

let find (p:'a -> bool) (l:'a t): 'a option =
  try
    Some (find p l)
  with Not_found ->
    None




let split_head_tail (lst: 'a t): 'a * 'a t =
    assert (lst <> []);
    match lst with
    | [] ->
        assert false (* Illegal call! *)
    | hd :: tl ->
        hd, tl





let map_and_filter (f:'a -> 'b option) (l:'a list): 'b list =
  let rec map = function
    | [] ->
       []
    | hd :: tl ->
       match f hd with
       | None ->
          map tl
       | Some b ->
          b :: map tl
  in
  map l



let split_at (p:'a -> bool) (l: 'a t): 'a t * 'a t =
  let rec split prefix rest =
    match rest with
    | [] ->
       rev prefix, rest
    | hd :: tl  ->
       if p hd then
         rev prefix, rest
       else
         split (hd :: prefix) tl
  in
  split [] l



let transpose (row_list: 'a list list): 'a list list =
    assert (row_list <> []);
    let first_column row_list =
        (* Extract the first column of [row_list]. *)
        fold_right
            (fun row (column, row_list)->
                match row with
                | [] ->
                    assert false
                | el :: rest_row ->
                    el :: column,
                    rest_row :: row_list
            )
            row_list
            ([], [])
    in
    let rec get_columns columns row_list =
        match row_list with
        | [] ->
            assert false (* No rows is not allowed. *)

        | [] :: _ ->
            columns

        | (_ :: _) :: _ ->
            let column, row_list = first_column row_list in
            get_columns (column :: columns) row_list
    in
    rev (get_columns [] row_list)





module Monadic (M: MONAD) =
  struct
    let foldi_left (f:int -> 'a -> 'b -> 'b M.t) (l:'a t) (start:'b)
        : 'b M.t =
      let rec foldi i l start =
        match l with
        | [] ->
           M.return start
        | hd :: tl ->
           M.(f i hd start >>= foldi (i+1) tl)
      in
      foldi 0 l start

    let fold_left (f:'a -> 'b -> 'b M.t) (l:'a t) (start:'b): 'b M.t =
      foldi_left (fun _ -> f) l start

    let fold_right (f:'a -> 'b -> 'b M.t) (l:'a t) (start:'b): 'b M.t =
      fold_left f (rev l) start
  end




(* Unit Tests *)

let%test _ =
    transpose [ [1] ] = [ [1] ]


let%test _ =
    transpose
        [ [1;2;3] ] = [ [1]; [2]; [3] ]

let%test _ =
    transpose
        [ [1;2;3]; [4;5;6] ] = [ [1;4]; [2;5]; [3;6] ]
