let two_bits:   int = 0b0000_0011
let three_bits: int = 0b0000_0111
let four_bits:  int = 0b0000_1111
let five_bits:  int = 0b0001_1111
let six_bits:   int = 0b0011_1111

let is_continuation (c: int): bool =
    c lsr 6 = 0b10


let is_1byte (c: int): bool =
    c lsr 7 = 0


let is_2byte (c: int): bool =
    c lsr 5 = 0b110


let is_3byte (c: int): bool =
    c lsr 4 = 0b1110


let is_4byte (c: int): bool =
    c lsr 3 = 0b11110



module Decoder =
struct
    type t =
        {
            value:   int;
            missing: int;
            length:  int;
        }


    let is_complete (u: t): bool =
        u.missing = 0


    let has_error (u: t): bool =
        u.value = -1


    let uchar (u: t): Uchar.t =
        if Uchar.is_valid u.value then
            Uchar.of_int u.value
        else
            Uchar.rep


    let scalar (u: t): int =
        if Uchar.is_valid u.value then
            u.value
        else
            Uchar.(to_int rep)


    let byte_width (u: t): int =
        u.length - u.missing


    let width (u: t): int =
        let i = scalar u in
        if i = Char.code '\t' then
            4
        else if i < Char.code ' ' then
            0
        else
            match i with
            | 0x200B        (* zero width space *)
            | 0xFEFF        (* zero-width no-break space (bom) *)
            | 0x200C        (* zero-width non-joiner *)
            | 0x200D        (* zero-width joiner *)
                -> 0
            | _
                -> 1


    let is_newline (u: t): bool =
        scalar u = 0x0A (* LF *)
        (*
        match scalar u with
        | 0x000A            (* LF line feed *)
        | 0x000B            (* VT vertical tab *)
        | 0x000C            (* FF form feed *)
        | 0x0085            (* NEL next line *)
        | 0x2028            (* LS line separator *)
        | 0x2029            (* PS paragraph separator *)
            -> true
        | _
            -> false*)





    let init: t =
        {
            value    = 0;
            missing  = 0;
            length   = 0;
        }


    let error (length: int): t =
        {
            value    = -1;
            missing  = 0;
            length;
        }


    let put (c: char) (u: t): t =
        let c = Char.code c
        in
        if u.missing = 0 then

            if is_1byte c then
                {
                    value    = c;
                    missing  = 0;
                    length   = 1;
                }
            else if is_2byte c then begin
                {
                    value    = five_bits land c;
                    missing  = 1;
                    length   = 2;
                }
            end
            else if is_3byte c then begin
                {
                    value    = four_bits land c;
                    missing  = 2;
                    length   = 3;
                }
            end
            else if is_4byte c then begin
                {
                    value    = three_bits land c;
                    missing  = 3;
                    length   = 4;
                }
            end
            else
                error 1

        else if u.missing > 0 then

            if is_continuation c then begin
                {
                    u with
                    value    = u.value lsl 6 + six_bits land c;
                    missing  = u.missing - 1;
                }
            end
            else
                error (u.length + 1)

        else
            assert false (* cannot happen *)


    let _: unit =
        assert (0x10FFFF lsr 21 = 0)
end



module Encoder =
struct
    type t = Uchar.t


    let to_string (scalar: int): string =
        assert (scalar <= 0x10FFFF);
        let open Printf in
        let open Char in

        if scalar < 0x8F then

            sprintf "%c" (chr scalar)

        else if scalar lsr 11 = 0 then

            let c1 = (two_bits lsl 6) lor (scalar lsr 6)  |> chr
            and c2 = (1 lsl 7) lor (scalar land six_bits) |> chr
            in
            sprintf "%c%c" c1 c2

        else if scalar lsr 16 = 0 then

            let c1 = (three_bits lsl 5) lor (scalar lsr 12)     |> chr
            and c2 = (1 lsl 7) lor (scalar lsr 6 land six_bits) |> chr
            and c3 = (1 lsl 7) lor (scalar land six_bits)       |> chr
            in
            sprintf "%c%c%c" c1 c2 c3

        else if scalar lsr 21 = 0 then

            let c1 = (four_bits lsl 4) lor (scalar lsr 18)       |> chr
            and c2 = (1 lsl 7) lor (scalar lsr 12 land six_bits) |> chr
            and c3 = (1 lsl 7) lor (scalar lsr  6 land six_bits) |> chr
            and c4 = (1 lsl 7) lor (scalar        land six_bits) |> chr
            in
            sprintf "%c%c%c%c" c1 c2 c3 c4

        else

            assert false (* cannot happen *)



    let to_internal (uc: t): string =
        to_string Uchar.(to_int uc)



    let to_external (uc: t): string =
        to_string Uchar.(to_int uc)
end










(* Unit Tests
   ============================================================
*)


let%test _ =
    is_continuation 0b1000_1000




let%test _ =
    is_1byte 0b0001_0000




let%test _ =
    not (is_continuation 0b1100_0000)


let%test _ =
    is_3byte 0xe2


let%test _ =
    is_continuation 0x81


let%test _ =
    is_continuation 0xba




(* Utf8 Strings *)

open Decoder
open Encoder


let consume_string (s: string) (u: Decoder.t): Decoder.t =
    let len = String.length s
    in
    let rec consume i u =
        if i = len then
            u
        else begin
            consume (i + 1) (put s.[i] u)
        end
    in
    consume 0 u



let%test _ =
    let u = consume_string "abc" init in
    is_complete u
    &&
    Uchar.is_char (uchar u)
    &&
    Uchar.to_char (uchar u) = 'c'





let%test _ =
    let u = consume_string "abc\u{207a}" init in
    is_complete u
    &&
    not (Uchar.is_char (uchar u))
    &&
    Uchar.to_int (uchar u) = 0x207a





let%test _ =
    let u = consume_string "\u{e000}" init in
    is_complete u
    &&
    not (Uchar.is_char (uchar u))
    &&
    Uchar.to_int (uchar u) = 0xe000





let%test _ =
    let s = "\u{10ffff}" in
    assert (String.length s = 4);
    assert (s.[0] = '\xf4');
    assert (s.[1] = '\x8f');
    assert (s.[2] = '\xbf');
    assert (s.[3] = '\xbf');
    assert (s = "\xf4\x8f\xbf\xbf");
    let u = consume_string s init in
    is_complete u
    &&
    not (Uchar.is_char (uchar u))
    &&
    Uchar.to_int (uchar u) = 0x10ffff



let%test _ =
    (* unicode code points must be in the ranges [0x0000 - 0xD7FF] and [0xE000 -
       0x10FFFF]. Here we construct a code point one above the highest.
     *)
    let u = consume_string "\xf5\x8f\xbf\xbf" init in
    is_complete u
    &&
    Uchar.(rep = uchar u)







(* Encoding
   ============================================================
*)


let%test _ =
    let open String
    in
    (
        to_string 0x00     = "\u{0}"
        &&
        to_string 0x7F     = "\u{7F}"
        &&
        to_string 0x0  |> length = 1
        &&
        0x7F = 127
        &&
        to_string 0x7F |> length = 1
    )
    &&
    (
        to_string 0x8F     = "\u{8F}"
        &&
        to_string 0x7FF    = "\u{7FF}"
        &&
        to_string 0x8F  |> length = 2
        &&
        to_string 0x7FF |> length = 2
    )
    &&
    (
        to_string 0x8FF    = "\u{8FF}"
        &&
        to_string 0xD7FF   = "\u{D7FF}"
        &&
        to_string 0xE000   = "\u{E000}"
        &&
        to_string 0xFEFF   = "\u{FEFF}"     (* bom *)
        &&
        to_string 0xFFFD   = "\u{FFFD}"     (* rep *)
        &&
        to_string 0xFFFF   = "\u{FFFF}"
        &&
        to_string 0x8FF  |> length = 3
        &&
        to_string 0xD7FF |> length = 3
        &&
        to_string 0xE000 |> length = 3
        &&
        to_string 0xFEFF |> length = 3
        &&
        to_string 0xFFFD |> length = 3
        &&
        to_string 0xFFFF |> length = 3
    )
    &&
    (
        to_string 0x1FFFF   = "\u{1FFFF}"
        &&
        to_string 0x10FFFF  = "\u{10FFFF}"
        &&
        to_string 0x1FFFF  |> length = 4
        &&
        to_string 0x10FFFF |> length = 4
    )
