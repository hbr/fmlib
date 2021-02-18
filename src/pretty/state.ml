open Std

(* A Text Block
 * ============
 *
 * A text block is never empty. It consists either of a (partial) string or an
 * instruction to fill the output a character repeated.
 *)

module Text =
struct
    type t =
        | String of string * int * int
        | Fill of int * char


    let substring (str: string) (start: int) (len: int): t =
        assert (0 <= start);
        assert (0 < len);
        assert (start + len <= String.length str);
        String (str, start, len)


    let string (str: string): t =
        let len = String.length str in
        assert (0 < len);
        substring str 0 len


    let fill (n: int) (c: char): t =
        assert (0 < n);
        Fill (n, c)


    let char (c: char): t =
        Fill (1, c)


    let length: t -> int = function
        | String (_, _, len) ->
            len
        | Fill (len, _) ->
            len


    let peek (text: t): char =
        match text with
        | String (s, start, _) ->
            assert (start < String.length s);
            s.[start]
        | Fill (_, c) ->
            c


    let advance (text: t): t option =
        match text with
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

    let to_string (text: t): string =
        match text with
        | String (s, start, len) ->
            String.sub s start len
        | Fill (len, c) ->
            String.make len c

    let _ = to_string (* might be needed during debugging *)
end







(* Line Formatting Data
 * ====================
 *)

module Line =
struct
    type t = {
        indent: int;
        width:  int;
        ribbon: int;
    }

    let make (indent: int) (width: int) (ribbon: int): t =
        {indent; width; ribbon;}

    let indent (l: t): int =
        l.indent

    let width (l: t): int =
        l.width

    let ribbon (l: t): int =
        l.ribbon

    let increment_indent (i: int) (l: t): t =
        assert (0 <= l.indent + i);
        {l with indent = l.indent + i}

    let set_width (width: int) (l: t): t =
        {l with width}

    let set_ribbon (ribbon: int) (l: t): t =
        {l with ribbon}
end







(* Groups and Chunks in the Buffer
 * ===============================
 *)

type
    chunk = {
        break_text:
            string;

        line: Line.t;       (* before the first text block. *)

        texts:
            Text.t Deque.t;

        chunk_groups:
            group Deque.t;
    }

and
    group = {
        glength: int;
        complete_groups: group Deque.t;
        chunks: chunk Deque.t;
    }




module Chunk =
struct
    type t = chunk

    let make (break_text: string) (line: Line.t): t =
        {
            break_text;
            line;
            texts = Deque.empty;
            chunk_groups = Deque.empty;
        }

    let push_text (text: Text.t) (chunk: t): t =
        (* If the chunk has already groups, no more text can be added. *)
        assert (Deque.is_empty chunk.chunk_groups);
        {
            chunk with

            texts =
                Deque.push_rear
                    text
                    chunk.texts
        }


    let update_line (line: Line.t) (chunk: t): t=
        assert (Deque.is_empty chunk.chunk_groups);

        if Deque.is_empty chunk.texts then
            {chunk with line}
        else
            chunk


    let add_group (group: group) (chunk: t): t =
        {
            chunk with
            chunk_groups =
                Deque.push_rear
                    group
                    chunk.chunk_groups;
        }


    let break_text (chunk: t): string =
        chunk.break_text


    let line (chunk: t): Line.t =
        chunk.line


    let texts (chunk: t): Text.t Deque.t =
        chunk.texts


    let groups (chunk: t): group Deque.t =
        chunk.chunk_groups
end



module Group =
struct
    type t = group

    let empty: t =
        {
            glength = 0;
            complete_groups = Deque.empty;
            chunks = Deque.empty;
        }


    let push_text (text: Text.t) (group: t): t =
        (* Text can only be pushed to a group if it has at least one chunk. *)
        assert (not (Deque.is_empty group.chunks));
        {
            group with
            chunks =
                Deque.update_last
                    (Chunk.push_text text)
                    group.chunks;
            glength =
                group.glength + Text.length text;
        }


    let push_break (str: string) (line: Line.t) (group: t): t =
        {
            group with

            chunks =
                Deque.push_rear
                    (Chunk.make str line)
                    group.chunks;

            glength =
                group.glength + String.length str;
        }


    let update_line (line: Line.t) (group: t): t =
        assert (not (Deque.is_empty group.chunks));
        {
            group with

            chunks =
                Deque.update_last
                    (Chunk.update_line line)
                    group.chunks;
        }


    let add_complete (complete_group: t) (group: t): t =
        if Deque.is_empty group.chunks then
            {
                group with

                complete_groups =
                    Deque.push_rear
                        complete_group
                        group.complete_groups;
            }
        else
            {
                group with

                chunks =
                    Deque.update_last
                        (Chunk.add_group complete_group)
                        group.chunks;
            }



    let length (group: t): int =
        group.glength


    let complete_groups (group: t): t Deque.t =
        group.complete_groups


    let chunks (group: t): Chunk.t Deque.t =
        group.chunks
end








(* Buffer
 * ======
 *)

module Buffer =
struct
    type t = {
        ngroups: int;
        length: int;
        groups: group list;
    }


    let empty: t =
        {
            ngroups = 0;
            length = 0;
            groups  = [];
        }


    let count (b: t): int =
        b.ngroups


    let is_empty (b: t): bool =
        b.ngroups = 0


    let length (b: t): int =
        b.length


    let push_text (text: Text.t) (buffer: t): t =
        assert (0 < count buffer);
        let hd, tl =
            List.split_head_tail buffer.groups
        in
        {
            buffer with

            groups =
                (Group.push_text text hd) :: tl;

            length =
                buffer.length + Text.length text;
        }


    let push_break (str: string) (line: Line.t) (b: t): t =
        assert (0 < count b);
        let hd, tl =
            List.split_head_tail b.groups
        in
        {
            b with

            groups =
                (Group.push_break str line hd) :: tl;

            length =
                b.length + String.length str
        }


    let close_one (buffer: t): t =
        match buffer.groups with
        | last :: previous :: groups ->
            {
                buffer with

                groups =
                    Group.add_complete last previous :: groups;

                ngroups =
                    buffer.ngroups - 1;
            }
        | _ ->
            assert false (* Illegal call! *)


    let close_and_open
            (nclose: int)
            (nopen: int)
            (str: string)
            (line: Line.t)
            (buffer: t)
        : t
        =
        assert (nclose = 0 || nclose < count buffer);
        assert (0 < count buffer + nopen - nclose);
        push_break
            str
            line
            {
                (Int.iterate nclose close_one buffer)
                with

                ngroups =
                    buffer.ngroups + nopen;

                groups =
                    Int.iterate
                        nopen
                        (fun gs -> Group.empty :: gs)
                        buffer.groups;
            }


    let update_line (line: Line.t) (buffer: t): t =
        match buffer.groups with
        | [] ->
            buffer
        | group :: groups ->
            {buffer with
             groups =
                 Group.update_line line group
                 ::
                 groups;}


    let reverse (b: t): t =
        {b with groups = List.rev b.groups}


    let pop (b: t): (Group.t * t) option =
        match b.groups with
        | [] ->
            None
        | g :: groups ->
            let len = Group.length g in
            assert (len <= b.length);
            Some (
                g,
                {ngroups = b.ngroups - 1;
                 length  = b.length - len;
                 groups }
            )
end





(* Pretty Printer State
 * ====================
 *)


type t = {
    user_line: Line.t;   (* Current line data from the user *)
    next_line: Line.t;

    line: Line.t;        (* This line *)
    position: int;       (* Current position on the line *)


    active_groups: int;    (* Number of open active groups *)
    effective_groups: int; (* Number of open effective groups *)
    right_groups: int;     (* Number of open groups to the right of the last
                              open group in the buffer *)

    buffer: Buffer.t;
}


let init (width: int) (ribbon: int): t =
    let line = Line.make 0 width ribbon
    in
    {
        position = 0;

        user_line = line;
        next_line = line;
        line;

        active_groups = 0;
        effective_groups = 0;
        right_groups = 0;

        buffer = Buffer.empty
    }



let line_indent (s: t): int =
    if s.position = 0 then
        Line.indent s.line
    else
        0


let advance_position (n: int) (s: t): t =
    assert ( s.position <> 0
             ||
             Line.indent s.line <= n);
    (* Positions between 0 and line_indent are illegal. *)
    {s with position = s.position + n}


let newline (s: t): t =
    {s with position = 0; line = s.next_line;}


let newline_with_line (line: Line.t) (s: t): t =
    {s with
     position = 0;
     line;
     next_line = line;}


let fits (n: int) (s: t): bool =
    (* Is it possible to put [n] more characters on the current line without
     * violating the line width and the ribbon size? *)
    if s.position = 0 then
        n <= Line.ribbon s.line
        &&
        Line.indent s.line + n <= Line.width s.line
    else
        begin
            assert (Line.indent s.line <= s.position);
            s.position - Line.indent s.line + n <= Line.ribbon s.line
            &&
            s.position + n <= Line.width s.line
        end







(* Groups and Buffering
 * ====================
 *)


let buffered_groups (s: t): int =
    (* Number of open groups in the buffer. *)
    Buffer.count s.buffer


let is_buffering (s: t): bool =
    0 < buffered_groups s


let direct_out (s: t): bool =
    not (is_buffering s)


let line_direct_out (s: t): bool =
    direct_out s
    &&
    s.active_groups = 0


let within_active (s: t): bool =
    0 < s.active_groups


let buffer_fits (s: t): bool =
    fits (Buffer.length s.buffer) s



let push_text (text: Text.t) (s: t): t =
    assert (is_buffering s);
    {s with
     buffer = Buffer.push_text text s.buffer}



let push_break (str: string) (s: t): t =
    (* Push a break hint to the buffer. At the end, the number of incomplete
     * groups in the buffer and the number of active groups must be the same
     * and the number of right groups must be zero. *)
    assert (within_active s);
    let oa = s.active_groups
    and nbuf = buffered_groups s
    in
    let nclose, nopen =
        if nbuf = 0 then
            (* Start buffering *)
            0,
            oa
        else if oa <= nbuf then
            (* The innermost [nbuf - oa] groups in the buffer have already been
             * closed by the user. We close these groups in the buffer as well
             * and open [right_groups] in the buffer there the last group has a
             * chunk with the break hint. *)
            nbuf - oa,
            s.right_groups
        else
            (* nbuf < oa *)
            assert false (* This case cannot happen. At the start of buffering
                            we have [nbuf = oa]. If more groups are opened, they
                            are all counted as [right_groups]. *)
    in
    {
        s with

        right_groups =
            0;

        active_groups =
            oa + s.right_groups;

        buffer =
            Buffer.close_and_open
                nclose
                nopen
                str
                s.user_line
                s.buffer;
    }



let pull_buffer (s: t): Buffer.t * t =
    Buffer.reverse s.buffer,
    {s with buffer = Buffer.empty}



let flatten_done (s: t): t =
    (* The complete buffer has been flattened. *)
    assert (not (is_buffering s));
    assert (s.active_groups = 0);
    {
        s with
        active_groups =
             s.right_groups;

        right_groups  =
             0;

        next_line =
            s.user_line
    }



let effective_done (buffer: Buffer.t) (nflushed: int) (s: t): t =
    (* The outermost [nflushed] groups have been flushed as effective groups.
       Now the buffer fits.
     *)
    assert (not (is_buffering s));
    assert (Buffer.is_empty buffer || fits (Buffer.length buffer) s);
    let buffer =
        Buffer.reverse buffer
    and nflushed =
        min nflushed s.active_groups (* Of the flushed groups, only the groups
                                        count which had been open initially. *)
    in
    let effective_groups, active_groups, right_groups =
        if Buffer.count buffer = 0 then
            (* All groups have been flushed, buffer is empty. *)
            begin
                s.effective_groups + nflushed,
                s.active_groups + s.right_groups - nflushed,
                0
            end
        else if 0 < s.active_groups then
            s.effective_groups + nflushed,
            s.active_groups    - nflushed,
            s.right_groups
        else
            begin
                assert (s.active_groups = 0);
                assert (nflushed = 0);
                s.effective_groups,
                s.right_groups,
                0
            end
    in
    {
        s with buffer; effective_groups; active_groups; right_groups;
    }







(* Enter and Leave Groups
 * ======================
 *)

let enter_group (s: t): t =
    if is_buffering s then
        {s with right_groups = s.right_groups + 1}
    else
        {s with active_groups = s.active_groups + 1}


let leave_group (s: t): t =
    if 0 < s.right_groups then
        {s with right_groups = s.right_groups - 1}
    else if 0 < s.active_groups then
        {s with active_groups = s.active_groups - 1}
    else
        begin
            assert (0 < s.effective_groups);
            {s with effective_groups = s.effective_groups - 1}
        end






(* Indenting and Line Info
 * =======================
 *)


let update_line (new_line: Line.t) (s: t): t =
    {
        s with

        user_line =
            new_line;

        line =
            if s.position = 0 then
                (* Nothing has been printed to the current line. The
                 * [line] has to be updated immediately. *)
                new_line
            else
                s.line;

        next_line =
            if direct_out s then
                (* In direct output mode the new indentation must become
                 * effective for the next line *)
                new_line
            else
                s.next_line;

        buffer =
            Buffer.update_line
                new_line
                s.buffer;
    }



let increment_indent (n: int) (s: t): t =
    assert (0 <= Line.indent s.user_line + n);
    update_line
        (Line.increment_indent n s.user_line)
        s


let width (width: int) (s: t): int * t =
    assert (0 <= width);
    Line.width s.user_line
    ,
    update_line
        (Line.set_width width s.user_line)
        s


let ribbon (ribbon: int) (s: t): int * t =
    assert (0 <= ribbon);
    Line.ribbon s.user_line
    ,
    update_line
        (Line.set_ribbon ribbon s.user_line)
        s
