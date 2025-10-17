let force = Lazy.force


module Text =
struct
    type t =
        | String of string * int * int
        | Fill of int * char
        | Line

    let substring (str: string) (start: int) (len: int): t =
        assert (0 <= start);
        assert (0 < len);   (* Must not be empty, otherwise [peek] is not
                               possible. *)
        assert (start + len <= String.length str);
        String (str, start, len)


    let fill (n: int) (c: char): t =
        assert (0 < n);
        Fill (n, c)


    let string s =
        let len = String.length s in
        assert (0 < len);
        substring s 0 len


    let line: t = Line


    let blanks n =
        assert (0 < n);
        fill n ' '


    let length (t: t): int =
        match t with
        | String (_, _, len) ->
            len
        | Fill (len, _) ->
            len
        | Line ->
            1

    let is_line (t: t): bool =
        t = Line


    let peek (t: t): char =
        match t with
        | String (s, start, _) ->
            assert (start < String.length s);
            s.[start]
        | Fill (_, c) ->
            c
        | Line ->
            '\n'


    let advance (t: t): t option =
        match t with
        | String (s, start, len) ->
            if 1 < len then
                Some (String (s, start + 1, len - 1))
            else
                None
        | Fill (len, c) ->
            if 1 < len then
                Some (Fill (len - 1, c))
            else
                None
        | Line ->
            None

    let to_string (t: t): string =
        match t with
        | String (s, start, len) ->
            String.sub s start len
        | Fill (len, c) ->
            String.make len c
        | Line ->
            String.make 1 '\n'

    let _ = to_string (* to_string might be needed for debug purposes *)
end



module Stream =
struct
    type t =
        | Done
        | More of Text.t * t Lazy.t


    let nil: t = Done


    let more t laz: t = More (t, laz)


    let more2 x y laz = More (x, lazy (More (y, laz)))


    let rec fits (w: int): t -> bool =
        function
        | _ when w < 0 -> false
        | Done         -> true
        | More (t, x)  ->
            Text.is_line t
            ||
            fits (w - Text.length t) (force x)


    let has_more (p: t): bool =
        match p with
        | Done ->
            false
        | _ ->
            true


    let peek (p: t): char =
        match p with
        | Done ->
            assert false (* Illegal call! *)

        | More (text, _) ->
            Text.peek text



    let advance (p: t): t =
        match p with
        | Done ->
            assert false (* Illegal call! *)

        | More (text, rest) ->
            match Text.advance text with
            | Some text ->
                More (text, rest)
            | None ->
                Lazy.force rest

    let write_to_channel (oc: out_channel) (r: t): unit =
        let rec write r =
            if has_more r then
                begin
                    output_char oc (peek r);
                    write (advance r)
                end
        in
        write r
end





type t =
    | Nil
    | Text  of Text.t
    | Line  of string    (* flattened text *)
    | Cat   of t Lazy.t * t Lazy.t
    | Nest  of int * t Lazy.t
    | Union of (t Lazy.t * t)


let rec flatten: t -> t Lazy.t = function
    | Nil as x ->
        lazy x

    | Line s ->
        if s = "" then
            lazy Nil
        else
            lazy (Text (Text.string s))

    | Text _ as x ->
        lazy x

    | Cat (x, y) ->
        lazy (Cat (force x |> flatten, force y |> flatten))

    | Nest (i, x) ->
        lazy (Nest (i, force x |> flatten))

    | Union (x, _) ->
        force x |> flatten





let rec best (w: int) (r: int) (k: int): (int * t) list -> Stream.t =
    let open Stream in
    function
    | [] ->

        nil

    | (i, x) :: z ->
        match x with
        | Nil ->

            best w r k z

        | Text t ->

            let k, more =
                if 0 < i && k = 0 then
                    i, more2 (Text.blanks i) t
                else
                    k, more t
            in
            more (lazy (best w r (k + Text.length t) z))

        | Line _ ->

            more Text.line (lazy (best w r 0 z))

        | Cat (x, y) ->

            best w r k ((i, force x) :: (i, force y) :: z)

        | Nest (j, x) ->

            best w r k ((i + j, force x) :: z)

        | Union (x, y) ->

            assert (k = 0 || i <= k);
            let max_len = min w (i + r) - max k 0
            and x = best w r k ((i, force x) :: z) in
            if fits max_len x then
                x
            else
                best w r k ((i, y) :: z)









(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Public Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

let empty: t = Nil

let text s =
    if 0 < String.length s then
        Text (Text.string s)
    else
        Nil


let substring s start len =
    assert (0 <= start);
    assert (0 <= len);
    assert (start + len <= String.length s);
    if len = 0 then
        Nil
    else
        Text (Text.substring s start len)


let fill n c =
    assert (0 <= n);
    if n = 0 then
        Nil
    else
        Text (Text.fill n c)


let break s   = Line s

let (<+>) x y = Cat (lazy x, lazy y)

let (+|)  x y = Cat (lazy x, y)

let (|+|) x y = Cat (x, y)

let nest i x  = Nest (i, lazy x)

let nest_lazy i x = Nest (i, x)

let group =
    function
    | Union _ as x -> x     (* group is idempotent *)
    | x            -> Union (flatten x, x)


let layout_with_ribbon (width: int) (ribbon: int) (x: t): Stream.t =
    best width ribbon 0 [0, x]


let layout (width: int) (x: t): Stream.t =
    layout_with_ribbon width width x


let write_to_channel (oc: out_channel) (r: Stream.t): unit =
    Stream.write_to_channel oc r


let to_string (r: Stream.t): string =
    let module From =
        Fmlib_std.String.From_source (Stream)
    in
    From.make r





(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Convenience Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let char c    = fill 1 c

let space     = break " "

let cut       = break ""


let rec cat: t list -> t = function
    | [] -> Nil
    | x :: xs ->
        x +| lazy (cat xs)


let separated_by (sep: t): t list -> t =
    let rec sep_by = function
        | []  -> Nil
        | [x] -> x
        | x :: xs -> x <+> sep +| lazy (sep_by xs)
    in
    sep_by


let pack (hint: string): t list -> t =
    separated_by (group (break hint))


let stack (hint: string): t list -> t =
    separated_by (break hint)


let stack_or_pack (hint: string) (xs: t list): t =
    stack hint xs |> group


let parent_child (hint: string) (i: int) (parent: t) (child: t): t =
    parent <+> nest i (break hint <+> group child) |> group


let paragraphs: t list -> t =
    separated_by cut



let wrap_words (s: string): t =
    let open Fmlib_std
    in
    let is_whitespace c =
        c = ' ' || c = '\n' || c = '\r' || c = '\t'
    in
    let not_whitespace c = not (is_whitespace c)
    in
    let word_start i =
        String.find not_whitespace i s
    and word_end i =
        String.find is_whitespace i s
    and len =
        String.length s
    in
    let rec from i =
        assert (i < len && not_whitespace s.[i]);
        let j = word_end i in
        let k = word_start j in
        assert (i < j);
        assert (j = len || is_whitespace s.[j]);
        assert (k = len || not_whitespace s.[k]);
        let d = substring s i (j - i) in
        if k = len then (* only whitespace after [d] *)
            d
        else
            (d <+> group space) +| lazy (from k)
    in
    let i = word_start 0 in
    if i = len then
        empty
    else
        from i


let rec wrap_words_list: string list -> t =
    function
    | []      -> empty
    | [x]     -> wrap_words x
    | x :: xs -> wrap_words x <+> group space +| lazy (wrap_words_list xs)
