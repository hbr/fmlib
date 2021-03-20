open Fmlib_std

(*

    A base64 character is one of [A-Za-z0-9+/].

    A complete base64 encoding is a sequence of base64 characters in groups of 4
    characters. At the end there can be a group of 2-4 characters optionally
    padded with '='. A group has at least 2 base64 characters.

    Each group of 2-4 character encode 1-3 characters.

    Becaues there are only 64 base64 characters, each base64 character can be
    viewed as an sextet i.e. as a number 0-63 or a 6 bit number.

    A base64 group of 2-4 sextets (i.e. 12-24 bits) encodes 1-3 bytes i.e.
    octets.  The encoding is done by the following scheme. Unsused parts of the
    sextets are padded with zero bits.

                 |  sextet1  |  sextet2  |  sextet3  |  sextet4  |
        encoded: |0 1 2 3 4 5|0 1 2 3 4 5|0 1 2 3 4 5|0 1 2 3 4 5|
        decoded: |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
                 |    octet1     |    octet2     |    octet4     |

*)


(* Convert a base64 character to an integer. *)
let to_int (c: char): int =
    if 'A' <= c && c <= 'Z' then
        Char.code c - Char.code 'A'
    else if 'a' <= c && c <= 'z' then
        Char.code c - Char.code 'a' + 26
    else if '0' <= c && c <= '9' then
        Char.code c - Char.code '0' + 52
    else if c = '+' then
        62
    else if c = '/' then
        63
    else
        assert false (* Illegal call! *)



(* Convert an integer < 64 to a base64 character. *)
let of_int (i: int): char =
    assert (0 <= i);
    assert (i < 64);
    if i < 26 then
        Char.(code 'A' + i |> chr)
    else if i < 52 then
        Char.(code 'a' + (i - 26) |> chr)
    else if i < 62 then
        Char.(code '0' + (i - 52) |> chr)
    else if i = 62 then
        '+'
    else
        '/'

(* Decode 2-4 sextets given as an integer array into a string of 1-3 characters.
*)
let decode (arr: int array): string =
    let len = Array.length arr in
    assert (2 <= len);
    assert (len <= 4);
    let one i =
        assert (i < 256);
        String.one (Char.chr i)
    in
    assert (arr.(0) < 64);
    assert (arr.(1) < 64);
    let octet1 = arr.(0) lsl 2 + arr.(1) lsr 4
    in
    if len = 2 then
        one octet1

    else begin
        assert (arr.(2) < 64);
        let octet2 = (arr.(1) land 0b1111) lsl 4 + arr.(2) lsr 2
        in
        if len = 3 then
            one octet1 ^ one octet2

        else begin
            assert (arr.(3) < 64);
            let octet3 = (arr.(2) land 0b11) lsl 6 + arr.(3)
            in
            one octet1 ^ one octet2 ^ one octet3
        end
    end




(* Encode a string of 1-3 bytes into a base64 group consiting of 2-4 sextets.
*)
let encode (str: string): int array =
    let len = String.length str in
    assert (1 <= len);
    assert (len <= 3);
    let sextet1 = Char.code str.[0] lsr 2
    and sextet2 = (Char.code str.[0] land 0b11) lsl 4
    in
    assert (sextet1 < 64);
    if len = 1 then
        [|sextet1; sextet2|]

    else
        let sextet2 = sextet2 + Char.code str.[1] lsr 4
        and sextet3 = (Char.code str.[1] land 0b1111) lsl 2
        in
        assert (sextet2 < 64);
        if len = 2 then
            [|sextet1; sextet2; sextet3|]

        else (* len = 3 *)
            let sextet3 = sextet3 + Char.code str.[2] lsr 6
            and sextet4 = Char.code str.[2] land 0b11_1111
            in
            assert (sextet3 < 64);
            [|sextet1; sextet2; sextet3; sextet4|]








let%test _ =
let rec check_from i =
    if i = 64 then
        true
    else begin
        i = to_int (of_int i)
        &&
        check_from (i + 1)
    end
in
check_from 0



let%test _ =
    encode "M" = [|to_int 'T'; to_int 'Q'|]



let%test _ =
    encode "Ma" = [|to_int 'T'; to_int 'W'; to_int 'E'|]


let%test _ =
    encode "Man" = [|to_int 'T'; to_int 'W'; to_int 'F'; to_int 'u'|]


let%test _ =
    decode (encode "M") = "M"




let%test _ =
    decode (encode "Ma") = "Ma"



let%test _ =
    decode (encode "Man") = "Man"
