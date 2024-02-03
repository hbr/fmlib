let high_surrogate_start = 0xD800
let high_surrogate_end   = 0xDBFF
let low_surrogate_start  = 0xDC00
let low_surrogate_end    = 0xDFFF

let other_planes_start   = 0x10000



module type TWO_BYTE =
sig
    val add: int -> int -> int
    val byte1: int -> int
    val byte2: int -> int
end

module Be2 =
struct
    let add (i1: int) (i2: int): int =
        (i1 lsl 8) + i2

    let byte1 (i: int): int =
        i lsr 8

    let byte2 (i: int): int =
        i land 0xFF
end

module Le2 =
struct
    let add (i1: int) (i2: int): int =
        i1 + (i2 lsl 8)

    let byte1 (i: int): int =
        i land 0xFF

    let byte2 (i: int): int =
        i lsr 8
end



module Encoder (Byte: TWO_BYTE) =
struct
    type t = Uchar.t

    let to_string (scalar: int): string =
        assert (scalar <= 0x10FFFF);
        let open Printf in
        let open Char   in
        let open Byte   in

        if scalar < other_planes_start then begin
            assert (
                scalar < high_surrogate_start
                ||
                low_surrogate_end < scalar
            );
            sprintf "%c%c" (byte1 scalar |> chr) (byte2 scalar |> chr)
        end
        else
            let i  = scalar - other_planes_start in
            let hi = high_surrogate_start + i lsr 10
            and lo = low_surrogate_start  + i land (1 lsl 10 - 1)
            in
            sprintf "%c%c%c%c"
                (byte1 hi |> chr) (byte2 hi |> chr)
                (byte1 lo |> chr) (byte2 lo |> chr)

    let to_internal (uc: t): string =
        Utf8.Encoder.to_internal uc

    let to_external (uc: t): string =
        to_string Uchar.(to_int uc)
end



module Decoder (Byte: TWO_BYTE) =
struct
    type t = {
        value:   int;
        w1:      int;
        w2:      int;
        missing: int;
        length:  int;
    }

    let is_complete (u: t): bool =
        u.missing = 0 && u.length > 0


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


    let init: t =
        {
            value    = 0;
            w1       = 0;
            w2       = 0;
            missing  = 0;
            length   = 0;
        }


    let error (length: int): t =
        {
            value    = -1;
            w1       = 0;
            w2       = 0;
            missing  = 0;
            length;
        }


    let put (c: char) (u: t): t =
        let c = Char.code c
        in
        if u.missing = 0 then
            {
                value   = c;
                w1      = c;
                w2      = c;
                missing = 1;
                length  = 2;
            }
        else if u.missing = 1 && u.length = 2 then

            let w1 = Byte.add u.w1 c
            in
            if w1 < high_surrogate_start || low_surrogate_end < w1 then
                {
                    u with
                    w1;
                    value   = w1;
                    missing = 0;
            }
            else if high_surrogate_start <= w1 && w1 <= high_surrogate_end then
                (* high surrogate *)
                {
                    u with
                    w1;
                    missing = 2;
                    length  = 4;
                }
            else
                error 2

        else if u.missing = 2 && u.length = 4 then
            {
                u with
                w2 = c;
                missing = 1;
            }

        else begin
            assert (u.missing = 1);
            assert (u.length  = 4);
            let w2 = Byte.add u.w2 c
            in
            if low_surrogate_start <= w2 && w2 <= low_surrogate_end then
                (* low surrogate *)
                {
                    u with
                    w2;
                    value =
                        (u.w1 - high_surrogate_start) lsl 10
                        +
                        (w2 - low_surrogate_start)
                        +
                        other_planes_start;
                    missing = 0;
                }
            else
                error 4
        end

    let run_on_string (str: string): t =
        let len = String.length str in
        let rec run i u =
            if i = len then
                u
            else
                run (i + 1) (put str.[i] u)
        in
        run 0 init


    let round_trip (str: string): string =
        let module Enc = Encoder (Byte) in
        let u = run_on_string str in
        Enc.to_external (uchar u)
end







module Be =
struct
    module Encoder = Encoder (Be2)

    module Decoder = Decoder (Be2)
end






module Le =
struct
    module Encoder = Encoder (Le2)

    module Decoder = Decoder (Le2)
end





(* Unit Tests
   ============================================================
*)



let ocaml_encode_be (uc: Uchar.t): string =
    let module B = Stdlib.Buffer in
    let buf = B.create 4 in
    B.add_utf_16be_uchar buf uc;
    B.contents buf



let ocaml_encode_le (uc: Uchar.t): string =
    let module B = Stdlib.Buffer in
    let buf = B.create 4 in
    B.add_utf_16le_uchar buf uc;
    B.contents buf


let round_trip_le (i: int) (len: int): bool =
    let uc = Uchar.of_int i in
    let str = ocaml_encode_le uc in
    assert (String.length str = len);
    str
    =
    Le.Decoder.round_trip str


let round_trip_be (i: int) (len: int): bool =
    let uc = Uchar.of_int i in
    let str = ocaml_encode_be uc in
    assert (String.length str = len);
    let res = Be.Decoder.round_trip str
    in
    str
    =
    res



let%test _ =
    round_trip_le 0x0 2


let%test _ =
    round_trip_le 0x10000 4



let%test _ =
    round_trip_be 0x0 2


let%test _ =
    round_trip_be 0x10000 4
