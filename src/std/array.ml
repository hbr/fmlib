include Stdlib.Array


type 'a t = 'a array


let push (x: 'a) (xs: 'a array): 'a array =
    let len = length xs in
    let xs_new = make (len + 1) x in
    blit xs 0 xs_new 0 len;
    xs_new
