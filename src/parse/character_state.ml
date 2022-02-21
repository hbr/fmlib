(* The state of the character parser.

   It encapsulates the current position, the indentation and the user state.
*)

open Fmlib_std.Interfaces


module Make (User: ANY) =
struct
    type t = {
        pos: Position.t;
        indent: Indent.t;
        user: User.t;
      }

    let make (pos: Position.t) (user: User.t): t =
        {
            pos;
            user;
            indent = Indent.initial
        }


    let position (s: t): Position.t =
        s.pos


    let line (s: t): int
        = Position.line s.pos


    let column (s: t): int =
        Position.column s.pos


    let user (s: t): User.t =
        s.user


    let put (user: User.t) (s: t): t =
        {s with user}


    let update (f: User.t-> User.t) (s: t) =
        {s with user = f s.user}


    let indent (s: t): Indent.t =
        s.indent


    let next (c: char) (s: t): t =
        {s with
         pos =
             Position.next c s.pos;
         indent =
             Indent.token (Position.column s.pos) s.indent
        }


    let check_position (s: t): Indent.violation option =
        Indent.check_position
            (Position.column s.pos)
            s.indent


    let align (s: t): t =
        {s with indent = Indent.align s.indent}


    let left_align (s: t): t =
        {s with indent = Indent.left_align s.indent}

    let end_align (s0: t) (s: t): t =
        {s with indent = Indent.end_align s0.indent s.indent}

    let start_detach (s:t): t =
        {s with indent = Indent.initial}


    let end_detach (s0:t) (s:t): t =
        {s with indent = s0.indent}


    let start_indent (i: int) (s: t): t =
        assert (0 <= i);
        {s with indent = Indent.start_indent i s.indent}


    let end_indent (i: int) (s0: t) (s: t): t =
        {s with indent = Indent.end_indent i s0.indent s.indent}
end
