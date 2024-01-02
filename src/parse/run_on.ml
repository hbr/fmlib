let string_at
        (needs_more: 'a -> bool)
        (put: char -> 'a -> 'a)
        (put_end: 'a -> 'a)
        (start: int)
        (str: string)
        (p: 'a)
    : int * 'a
    =
    let len = String.length str
    in
    assert (start <= len);
    let rec run i p =
        if i > len || not (needs_more p) then

            i, p

        else if i < len then

            run (i + 1) (put str.[i] p)

        else (* i = len *)

            run (i + 1) (put_end p)

    in
    run start p



let string
        (needs_more: 'a -> bool)
        (put: char -> 'a -> 'a)
        (put_end: 'a -> 'a)
        (str: string)
        (p: 'a)
    : 'a
    =
    string_at needs_more put put_end 0 str p |> snd





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
