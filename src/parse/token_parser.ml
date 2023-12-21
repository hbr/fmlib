open Fmlib_std.Interfaces


module State (User: ANY) =
struct
    type t = {
        indent: Indent.t;
        user:   User.t;
        pos1:   Position.t;
        pos2:   Position.t;
        consumed: bool;
    }

    let make (user: User.t): t =
        {indent = Indent.initial;
         user;
         pos1 = Position.start;
         pos2 = Position.start;
         consumed = false;}


    let check_position (column: int) (s: t): Indent.expectation option =
        Indent.check_position column s.indent


    let next (pos1: Position.t) (pos2: Position.t) (user: User.t) (s: t): t =
        {
            user;
            indent =
                Indent.token (Position.column pos1) s.indent;
            pos1 =
                if s.consumed then
                    s.pos1
                else
                    pos1;
            pos2;
            consumed = true;
        }


    let range (s: t): Position.range =
        s.pos1, s.pos2


    let start_located (s: t): t =
        {s with pos1 = s.pos2; consumed = false}

    let end_located (s0: t) (s: t): t =
        if s0.consumed then
            {s with pos1 = s0.pos1}
        else
            s


    let user (s: t): User.t =
        s.user


    let put (user: User.t) (s: t): t =
        {s with user}


    let update (f: User.t -> User.t) (s: t): t =
        {s with user = f s.user}


    let align (s: t): t =
        {s with indent = Indent.align s.indent}


    let left_align (s: t): t =
        {s with indent = Indent.left_align s.indent}


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







module Make
        (User:     ANY)
        (Token:    ANY)
        (Final:    ANY)
        (Semantic: ANY)
=
struct

    module State = State (User)

    module Tok =
    struct
        type t = Token.t Located.t
    end

    module Expect =
    struct
        type t = String.t * Indent.violation option
    end


    module Basic = Generic.Make (Tok) (State) (Expect) (Semantic) (Final)

    include Basic

    module Parser =
    struct
        module P0 = Basic.Parser
        include P0

        type state = User.t

        let state (p: t): User.t =
            P0.state p |> State.user
    end


    type expect   = String.t
    type state    = User.t
    type semantic = Semantic.t


    let map_and_update (f: User.t -> 'a -> 'b * User.t) (p: 'a t): 'b t =
        Basic.map_and_update
            (fun state a ->
                 let b, user = f (State.user state) a in
                 b, State.put user state)
            p


    let unexpected (e: String.t): 'a t =
        Basic.unexpected (e, None)


    let (<?>) (p: 'a t) (e: String.t): 'a t =
        Basic.(
            update_expectations
                (fun state -> function
                     | None ->
                         (* end of input reached *)
                         (e, None)
                     | Some ((p1, _), _) ->
                         let column = Position.column p1 in
                         (e, State.check_position column state)
                )
                p
        )


    let get: User.t t =
        Basic.(map State.user get)


    let update (f: User.t -> User.t): unit t =
        Basic.update (State.update f)


    let set (user: User.t): unit t =
        update (fun _ -> user)


    let get_and_update (f: User.t -> User.t): User.t t =
        Basic.(
            map
                State.user
                (get_and_update (State.update f))
        )

    let state_around
            (before: User.t -> User.t)
            (p: 'a t)
            (after: User.t -> 'a -> User.t -> User.t)
        : 'a t
        =
        Basic.state_around
            (State.update before)
            p
            (fun s0 a s1 -> State.(update (after (user s0) a) s1))


    let backtrack (p: 'a t) (e: String.t): 'a t =
        Basic.( backtrack p (e, None) )


    let followed_by (p: 'a t) (e: String.t): 'a t =
        Basic.( followed_by p (e, None) )


    let not_followed_by (p: 'a t) (e: String.t): unit t =
        Basic.( not_followed_by p (e, None) )


    let expect_end (a: 'a): 'a t =
        Basic.expect_end (fun _ -> "end of input", None) a


    let step
            (expect: String.t)
            (f: User.t -> Position.range -> Token.t -> ('a * User.t) option)
        : 'a Basic.t
        =
        Basic.step
            (fun state -> function
                 | None ->
                     (* end of input reached *)
                     Error (expect, None)

                 | Some ((p1, p2), tok) ->
                     let column = Position.column p1
                     in
                     match State.check_position column state with
                     | None ->
                         (* position ok *)
                         begin
                             match f (State.user state) (p1,p2) tok with
                             | None ->
                                 Error (expect, None)
                             | Some (a, user) ->
                                 Ok (a, State.next p1 p2 user state)
                         end

                     | Some vio ->
                         (* violated indentation expectation *)
                         Error (expect, Some vio)
            )





    (* Location Combinators
     * ====================
     *)


    let located (p: 'a t): (Position.range * 'a) t =
        let* s0 = Basic.get_and_update State.start_located in
        let* a  = p in
        let* s1 = Basic.get_and_update (State.end_located s0) in
        return (State.range s1, a)



    (* Indentation combinators *)

    let indent (i: int) (p: 'a t): 'a t =
        assert (0 <= i);
        let* state = Basic.get_and_update (State.start_indent i) in
        let* a     = p in
        let* _     = Basic.update (State.end_indent i state) in
        return a


    let align (p:'a t): 'a t =
        let* _ = Basic.update State.align in
        p


    let left_align (p:'a t): 'a t =
        let* _ = Basic.update State.left_align in
        p


    let detach (p: 'a t): 'a t =
        let* state = Basic.get_and_update State.start_detach in
        let* a     = p in
        let* _     = Basic.update (State.end_detach state) in
        return a



    (* Make the final parser *)

    let make (user: User.t) (p: Final.t t): Parser.t =
        Basic.make
            (State.make user)
            p
            (fun _ -> "end of input", None)


    let make_with_optional_end (user: User.t) (p: Final.t t): Parser.t =
        Basic.make_with_optional_end
            (State.make user)
            p
            (fun _ -> "end of input", None)


    let make_partial (user: User.t) (p: Final.t t): Parser.t =
        Basic.make_partial
            (State.make user)
            p
end
