let string
        (needs_more: 'a -> bool)
        (put: char -> 'a -> 'a)
        (put_end: 'a -> 'a)
        (str: string)
        (p: 'a)
    : 'a
    =
    let len = String.length str
    in
    let rec run i p =
        if not (needs_more p) then
            p
        else if i = len then
            put_end p
        else
            run (i + 1) (put str.[i] p)
    in
    run 0 p

let channel
        (needs_more: 'a -> bool)
        (put: char -> 'a -> 'a)
        (put_end: 'a -> 'a)
        (ic: in_channel)
        (p: 'a)
    : 'a
    =
    let rec run p =
        if not (needs_more p) then
            p
        else
            try
                run (put (input_char ic) p)
            with End_of_file ->
                put_end p
    in
    run p
