include Stdlib.Int


let iterate (n: t) (f: 'a -> 'a) (start: 'a): 'a =
    let rec iter n v =
        if n = 0 then
            v
        else
            iter (n - 1) (f v)
    in
    iter n start
