include Stdlib.String

let one (c:char): string =
    make 1 c




let is_prefix (a: string) (b:string): bool =
    let len_a = length a in
    len_a <= length b && a = sub b 0 len_a



let is_suffix (a: string) (b:string): bool =
    let len_a = length a
    and len_b = length b
    in
    len_a <= len_b
    && a = sub b  (len_b - len_a) len_a





let find (f:char -> bool) (start:int) (s:string): int =
    let len = length s in
    let rec find i =
        if i = len || f (get s i) then
            i
        else
            find (i+1)
    in
    find start


let has (f: char -> bool) (start: int) (s: string): bool =
    find f start s
    <
    length s


let find_bwd (f: char -> bool) (beyond: int) (s: string): int =
    let len = length s
    in
    let beyond =
        if beyond < 0 || len < beyond then
            len
        else
            beyond
    in
    let rec find i =
        if i = 0 || f (get s (i - 1)) then
            i - 1
        else
            find (i - 1)
    in
    find beyond



let list (s:string): char list =
    let rec list cs i =
        if i = 0 then
            cs
        else
            let j = i - 1 in
            list (get s j :: cs) j
    in
    list [] (length s)


let of_list (cs:char list): string =
    let rec str cs i =
        match cs with
        | [] ->
            Bytes.create i
        | c::cs ->
            let bs = str cs (i+1) in
            Bytes.set bs i c;
            bs
    in
    let bs = str cs 0 in
    Bytes.unsafe_to_string bs



let reverse (s: string): string =
    let len = length s in
    init len (fun i -> get s (len - 1 - i))


module To_source =
struct
    type item = char

    type t = int * string

    let has_more ((i,s): t): bool =
        i < length s

    let peek ((i,s): t): char =
        assert (has_more (i,s));
        get s i

    let advance ((i,s)): t =
        assert (has_more (i, s));
        (i + 1, s)

    let make (s: string): t =
        (0, s)
end



module From_source (R: Interfaces.SOURCE with type item := char) =
struct
    let make_with_size (estimate: int) (r: R.t): t =
        let estimate =
            if estimate <= 0 then
                80
            else
                estimate
        in
        let buffer   = ref (Bytes.create estimate)
        and len      = ref 0
        and capacity = ref estimate
        in
        let make_room () =
            if !len = !capacity then
                let bnew = Bytes.create (2 * !capacity) in
                begin
                    Bytes.blit !buffer 0 bnew 0 !len;
                    buffer := bnew;
                    capacity := 2 * !capacity
                end
        in
        let push c =
            make_room ();
            Bytes.set !buffer !len c;
            len := !len + 1;
        in
        let rec recurse r =
            if R.has_more r then
                begin
                    push (R.peek r);
                    recurse (R.advance r)
                end
            else
                Bytes.sub_string !buffer 0 !len
        in
        recurse r

    let make (r: R.t): t =
        make_with_size 100 r
end





let%test _ =
    let str = "12345678901234567890123456789012" in
    let module From = From_source (To_source) in
    str = From.make_with_size 2 (To_source.make str)
