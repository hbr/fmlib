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
    assert (start <= len + 1);
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





module Make (CD: Interfaces.CHAR_DECODER) =
struct
    let string_at
            (needs_more: 'a -> bool)
            (put: CD.t -> 'a -> 'a)
            (put_end: 'a -> 'a)
            (start: int)
            (str: string)
            (p: 'a)
        : int * 'a
        =
        let len = String.length str
        in
        assert (start <= len + 1);
        let rec run i d p =
            if i > len || not (needs_more p) then

                i, p

            else if i < len then

                let d = CD.put str.[i] d in
                let p =
                    if CD.(is_complete d || has_error d) then
                        put d p
                    else
                        p
                in
                run (i + 1) d p

            else (* i = len *)

                let p =
                    if CD.(is_complete d || has_error d) then
                        p
                    else
                        put d p
                in
                run (i + 1) d (put_end p)

        in
        run start CD.init p




    let string
            (needs_more: 'a -> bool)
            (put: CD.t -> 'a -> 'a)
            (put_end: 'a -> 'a)
            (str: string)
            (p: 'a)
        : 'a
        =
        string_at needs_more put put_end 0 str p |> snd





    let channel
            (needs_more: 'a -> bool)
            (put: CD.t -> 'a -> 'a)
            (put_end: 'a -> 'a)
            (ic: in_channel)
            (p: 'a)
        : 'a
        =
        let rec run d p =
            if not (needs_more p) then
                p
            else
                try
                    let c = input_char ic in
                    let d = CD.put c d    in
                    let p =
                        if CD.(is_complete d || has_error d) then
                            put d p
                        else
                            p
                    in
                    run d p
                with End_of_file ->
                    let p =
                        if CD.(is_complete d || has_error d) then
                            p
                        else
                            put d p
                    in
                    put_end p
        in
        run CD.init p
end
