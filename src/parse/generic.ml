open Fmlib_std
open Interfaces




module Make
         (Token: ANY)
         (State: ANY)
         (Expect: ANY)
         (Semantic: ANY)
         (Final: ANY)
=
struct
    module B =
        Buffer.Make (State) (Token) (Expect) (Semantic)

    module Error =
        Error.Make (Expect) (Semantic)


    module Parser =
    struct
        type token    = Token.t
        type item     = token
        type final    = Final.t
        type expect   = Expect.t
        type semantic = Semantic.t
        type state    = State.t

        type t =
            | More  of B.t * (B.t -> t)
            | Done of B.t * Final.t option


        let needs_more (p: t): bool =
            match p with
            | More _ ->
                true
            | Done _ ->
                false


        let has_ended  (p: t): bool =
            not (needs_more p)


        let has_succeeded (p: t): bool =
            match p with
            | Done (_, Some _) ->
                true
            | _ ->
                false

        let final (p: t): Final.t =
            assert (has_succeeded p);
            match p with
            | Done (_, Some v) ->
                v
            | _ ->
                assert false (* Illegal call. *)


        let has_failed_syntax (p: t): bool =
            match p with
            | Done (b, None) when Error.is_syntax (B.error b) ->
                true
            | _ ->
                false


        let has_failed_semantic (p: t): bool =
            match p with
            | Done (b, None) when Error.is_semantic (B.error b) ->
                true
            | _ ->
                false


        let failed_expectations (p: t): Expect.t list =
            assert (has_failed_syntax p);
            match p with
            | Done (b, None) when Error.is_syntax (B.error b) ->
                Error.expectations (B.error b)
            | _ ->
                assert false


        let failed_semantic (p: t): Semantic.t =
            assert (has_failed_semantic p);
            match p with
            | Done (b, None) when Error.is_semantic (B.error b) ->
                Error.semantic (B.error b)
            | _ ->
                assert false


        let rec consume_lookaheads (p: t): t =
            (* As long as there are lookaheads in the buffer, consume them. *)
            match p with
            | More (b, f) when B.has_lookahead b ->
                consume_lookaheads (f b)
            | _ ->
                p


        let put (t: Token.t) (p: t): t =
            let put = function
                | More (b, f) ->
                    More (B.push_token t b, f)

                | Done (b, res) ->
                    Done (B.push_token t b, res)
            in
            put p |> consume_lookaheads


        let put_end (p: t): t =
            let put = function
                | More (b, f) ->
                    More (B.push_end b, f)

                | Done (b, res) ->
                    Done (B.push_end b, res)
            in
            put p |> consume_lookaheads


        let buffer (p: t): B.t =
            match p with
            | More (b, _) | Done (b, _) ->
                b

        let state (p: t): State.t =
            buffer p |> B.state


        let has_lookahead (p: t): bool =
            buffer p |> B.has_lookahead

        let first_lookahead_token (p: t): Token.t option =
            buffer p |> B.first_lookahead


        let has_received_end (p: t): bool =
            buffer p |> B.has_end


        let has_consumed_end (p: t): bool =
            buffer p |> B.has_consumed_end


        let fold_lookahead
                (a: 'a)
                (ftok: Token.t -> 'a -> 'a)
                (fend: 'a -> 'a)
                (p: t)
            : 'a
            =
            buffer p |> B.fold_lookahead a ftok fend



        let transfer_lookahead (p_old: t) (p_new: t): t =
            fold_lookahead p_new put put_end p_old


        let lookaheads (p: t): Token.t array * bool =
            let b = buffer p in
                B.lookaheads b,
                B.has_end b
    end





    (* The parsing combinators
       -----------------------

       A parsing combinator is a continuation monad with state. The state is the
       parse buffer.
     *)

    type state = State.t
    type expect = Expect.t
    type semantic = Semantic.t

    type 'a cont =
        'a option -> B.t -> Parser.t

    type 'a t =
        B.t -> 'a cont -> Parser.t


    let return (a: 'a): 'a t =
        fun b k ->
        k (Some a) b


    let succeed (a:'a): 'a t =
        fun b k ->
        k (Some a) (B.clear_errors b)


    let clear_last_expectation (a:'a): 'a t =
        fun b k ->
        k (Some a) (B.clear_last_error b)


    let fail (e: Semantic.t): 'a t =
        fun b k ->
        k None (B.put_error e b)


    let unexpected (exp: Expect.t): 'a t =
        fun b k ->
        k None (B.add_expected exp b)


    let (>>=) (p: 'a t) (f: 'a -> 'b t): 'b t =
        fun b k ->
        p
            b
            (fun o b ->
                 match o with
                 | Some a ->
                     f a b k
                 | None ->
                     k None b)

    let ( let* ) = (>>=)


    let map_and_update (f: State.t -> 'a -> 'b * State.t) (p: 'a t): 'b t =
        fun buf k ->
        p
            buf
            (fun a_opt buf ->
                 match a_opt with
                 | None ->
                     k None buf
                 | Some a ->
                     let b, state = f (B.state buf) a in
                     k (Some b) (B.put state buf)
            )


    let map (f: 'a -> 'b) (p: 'a t): 'b t =
        map_and_update
            (fun state a -> f a, state)
            p



    let update (f: State.t -> State.t): unit t =
        fun b k ->
        k (Some ()) (B.update f b)


    let get: State.t t =
        fun b k ->
        k (Some (B.state b)) b


    let set (state: State.t): unit t =
        fun b k ->
        k (Some ()) (B.put state b)


    let get_and_update (f: State.t -> State.t): State.t t =
        fun b k ->
        let st = B.state b in
        k (Some st) (B.update f b)


    let state_around
            (before: State.t -> State.t)
            (p: 'a t)
            (after: State.t -> 'a -> State.t -> State.t)
        : 'a t
        =
        fun b0 k ->
        p
            (B.update before b0)
            (fun res b ->
                 match res with
                 | None ->
                     k None b
                 | Some a ->
                     k res B.(update (after (state b0) a) b))




    (* Basic Combinators
       ----------------------------------------
     *)



    let step
            (f: State.t -> Token.t option -> ('a * State.t, Expect.t) result)
        : 'a t
        =
        (* Basic parsing combinator which handles one token.

           The handling function [f] receives the current state and the current
           lookahead token and return a result on how to handle it.

           Success case: An item and a new state.

           Error case: A message of what has been expected by the combinator.
        *)
        fun b k ->
        More (b,
              fun b ->
                  assert (B.has_lookahead b || B.has_end b);
                  match f (B.state b) (B.first_lookahead b)  with
                  | Ok (a, s1) ->
                      k (Some a) (B.consume s1 b)
                  | Error e ->
                      k None (B.reject e b))


    let expect_end (e: State.t -> Expect.t) (a: 'a): 'a t =
        step
            (fun state token ->
                 match token with
                 | None ->
                     Ok (a, state)
                 | Some _ ->
                     Error (e state))




    let consumer (p: 'a t): 'a t =
        (* Execute [p].

           Precondition: [p] must consume at least one token in case of success.
        *)
        fun b0 k ->
        p
            (B.start_new_consumer b0)
            (fun res b ->
                 let consumed = B.has_consumed b in
                 assert (res = None || consumed);
                 k res (B.end_new_consumer b0 b))



    let (</>) (p: 'a t) (q: 'a t): 'a t =
        (* Try [p]. If it fails without consuming token, then try [q ()]. *)
        fun b0 k ->
        p
            (B.start_new_consumer b0)
            (fun res b ->
                 let consumed = B.has_consumed b in
                 let b = B.end_new_consumer b0 b in
                 match res with
                 | None when not consumed ->
                     (* p failed and did not consume token *)
                     q b k
                 |  _ ->
                     (* p did consume token and succeeded or failed. *)
                     k res b)



    let no_expectations (p: 'a t): 'a t =
        fun b0 k ->
        p
            (B.start_new_consumer b0)
            (fun res b ->
                 let consumed = B.has_consumed b in
                 let b = B.end_new_consumer b0 b in
                 match res with
                 | Some _ when not consumed ->
                     k res (B.reset_errors b0 b)
                 | Some _ ->
                     k res (B.clear_errors b)
                 | _ ->
                     k res b
            )

    let update_expectations
            (f: State.t -> Token.t option -> Expect.t)
            (p: 'a t)
        : 'a t
        =
        fun b0 k ->
        p
            (B.start_alternatives b0)
            (fun res b ->
                 match res with
                 | None ->
                     k None (B.end_failed_alternatives f b0 b)

                 | Some a ->
                     k (Some a) (B.end_succeeded_alternatives b0 b)
            )


    let (<?>) (p: 'a t) (e: Expect.t): 'a t =
        update_expectations (fun _ _ -> e) p
        (*fun b0 k ->
        p
            (B.start_alternatives b0)
            (fun res b ->
                 match res with
                 | None ->
                     k None (B.end_failed_alternatives e b0 b)
                 | Some a ->
                     k (Some a) (B.end_succeeded_alternatives b0 b))*)


    let backtrack (p: 'a t) (e: Expect.t): 'a t =
        fun b0 k ->
        p (B.start_backtrack b0)
            (fun res b ->
                 k res
                     (match res with
                      | None   -> B.end_backtrack_fail (Some e) b0 b
                      | Some _ -> B.end_backtrack_success b0 b))


    let not_followed_by (p: 'a t) (exp: Expect.t): unit t =
        fun b0 k ->
        p (B.start_backtrack b0)
            (fun res b ->
                 match res with
                 | None ->
                     k (Some ()) (B.end_backtrack_fail None b0 b)
                 | Some _ ->
                     k None (B.end_backtrack_fail (Some exp) b0 b))


    let followed_by (p: 'a t) (exp: Expect.t): 'a t =
        fun b0 k ->
        p
            (B.start_backtrack b0)
            (fun res b ->
                 match res with
                 | None ->
                     k None (B.end_backtrack_fail (Some exp) b0 b)
                 | Some a ->
                     k (Some a) (B.end_backtrack_fail None b0 b))






    (* Making Final Parsers
       ----------------------------------------
     *)




    let make_partial (s: State.t) (c: Final.t t): Parser.t =
        c
            (B.init s)
            (fun res b -> Done (b, res))


    let make (state: State.t) (p: Final.t t) (e: State.t -> Expect.t)
        : Parser.t
        =
        make_partial
            state
            (p >>= expect_end e)






    (* Convenience Combinators
       ----------------------------------------
     *)




    let optional (p: 'a t): 'a option t =
        (
            let* a = p in
            return (Some a)
        )
        </>
        return None


    let rec choices (p: 'a t) (qs: 'a t list): 'a t =
        match qs with
        | [] ->
            p
        | q :: qs ->
            choices (p </> q) qs


    let zero_or_more_fold_left
            (start: 'r)
            (f: 'r -> 'a -> 'r t)
            (p: 'a t)
        : 'r t
        =
        let rec many r =
            (
                let* a = p in
                let* r = f r a in
                many r
            )
            </>
            return r
        in
        many start


    let one_or_more_fold_left
            (first: 'a -> 'r t)
            (f: 'r -> 'a -> 'r t)
            (p: 'a t)
        : 'r t
        =
        let* r = p >>= first in
        zero_or_more_fold_left r f p




    let zero_or_more (p: 'a t): 'a list t =
        let rec many () =
            (
                let* a = p in
                let* lst = many () in
                return (a :: lst)
            )
            </> return []
        in
        many ()


    let one_or_more (p: 'a t): ('a * 'a list) t =
        let* x = p in
        let* xs = zero_or_more p in
        return (x, xs)


    let skip_zero_or_more (p: 'a t): int t =
        zero_or_more_fold_left 0 (fun i _ -> return (i + 1)) p


    let skip_one_or_more (p: 'a t): int t =
        let* _ = p in
        let* n = skip_zero_or_more p in
        return (n + 1)


    let one_or_more_separated
            (first: 'item -> 'r t)
            (next:  'r -> 'sep  -> 'item -> 'r t)
            (p: 'item t)
            (sep: 'sep t)
        : 'r t
        =
        let rec many r =
            (
                let* s    = sep in
                let* item = p in
                next r s item >>= many
            )
            </>
            return r
        in
        let* res = p >>= first in
        many res


    let counted
            (min: int)
            (max: int)
            (start: 'r)
            (next: int -> 'item -> 'r -> 'r)
            (p: 'item t)
        : 'r t
        =
        assert (0 <= min);
        assert (min <= max);
        let rec many i r =
            if i = max then
                return r
            else
                let pp =
                    let* a = p in
                    many (i + 1) (next (i + 1) a r)
                in
                if min <= i then
                    pp </> return r
                else
                    pp
        in
        many 0 start





    (* Parenthesized
     * -------------
     *)


    let parenthesized
            (mk: 'lpar -> 'a -> 'rpar -> 'b t)
            (lpar: 'lpar t)
            (p:     unit -> 'a t)
            (rpar: 'lpar -> 'rpar t)
        : 'b t
        =
        let* lp = lpar in
        let* a  = p () in
        let* rp = rpar lp in
        mk lp a rp



    (* Operator expressions
       --------------------

       Binding power:

            o1 > o2     o1 has more binding power to the left of o2

    *)
    let operator_expression
            (primary: 'exp t)
            (unary_operator: 'op t option)
            (binary_operator: 'op t)
            (is_left: 'op -> 'op -> bool t)
            (make_unary: 'op -> 'exp -> 'exp t)
            (make_binary: 'exp -> 'op -> 'exp -> 'exp t)
        : 'exp t
        =
        let primary         = consumer primary
        and unary_operator  = Option.map consumer unary_operator
        and binary_operator = consumer binary_operator
        in
        let rec apply_unaries us a =
            match us with
            | [] ->
                return a
            | u :: us ->
                apply_unaries us a
                >>=
                make_unary u
        in
        let rec expression_0 us =
            match unary_operator with
            | None ->
                assert (us = []);
                primary >>= expression_1
            | Some unary_operator ->
                (
                    let* u = unary_operator in
                    expression_0 (u :: us)
                )
                </>
                (
                    let* a = primary in
                    apply_unaries us a >>= expression_1
                )
        and expression_1 a =
            (
                let* o = binary_operator in
                expression_2 a o []
            )
            </>
            return a

        and expression_2 a o us =
            match unary_operator with
            | None ->
                assert (us = []);
                primary >>= expression_3 a o us
            | Some unary_operator ->
                (
                    let* u = unary_operator in
                    expression_2 a o (u :: us)
                )
                </>
                (
                    primary >>= expression_3 a o us
                )
        and expression_3 a o1 us b =
            (
                let* o2 = binary_operator in
                expression_4 a o1 us b o2
            )
            </>
            let* b = apply_unaries us b in
            make_binary a o1 b

        and expression_4 a o1 us b o2 =
            match us with
            | [] ->
                expression_5 a o1 b o2
            | u :: us_rest ->
                let* left = is_left u o2 in
                if left then
                    let* b = make_unary u b in
                    expression_4 a o1 us_rest b o2
                else
                    let* e, op3 = operand_0 u b o2 [] in
                    let* e = make_unary u e in
                    match op3 with
                    | None ->
                        make_binary a o1 e
                    | Some o3 ->
                        expression_4 a o1 us_rest e o3

        and expression_5 a o1 b o2 =
            let* left = is_left o1 o2 in
            if left then
                let* a = make_binary a o1 b in
                expression_2 a o2 []
            else
                let* e, op3 = operand_0 o1 b o2 [] in
                let* e = make_binary a o1 e in
                match op3 with
                | None ->
                    return e
                | Some o3 ->
                    expression_2 e o3 []

        and operand_0 o1 b o2 us =
            (* o1 < o2
                complete [b o2 us] to [e] or [e o3] such that [o1 > o3]

                Note: [o1] can be unary!
            *)
            let without_unary =
                let* c = primary in
                (
                    let* o3 = binary_operator in
                    operand_1 o1 b o2 us c o3
                )
                </>
                (
                    let* e =
                        apply_unaries us c
                        >>=
                        make_binary b o2
                    in
                    return (e, None)
                )
            in
            match unary_operator with
            | None ->
                assert (us = []);
                without_unary
            | Some unary_operator ->
                (
                    let* u = unary_operator in
                    operand_0 o1 b o2 (u :: us)
                )
                </>
                without_unary

        and operand_1 o1 b o2 us c o3 =
            (* o1 < o2 *)
            let _ = o1, b, o2, us, c, o3 in
            match us with
            | [] ->
                operand_2 o1 b o2 c o3
            | u :: us ->
                operand_3 o1 b o2 us u c o3

        and operand_2 o1 b o2 c o3 =
            (* o1 < o2 *)
            let* left = is_left o2 o3 in
            if left then
                let* e = make_binary b o2 c in
                let* left = is_left o1 o3 in
                if left then
                    return (e, Some o3)
                else
                    operand_0 o1 e o3 []
            else
                (* o2 < o3 *)
                let* e, op4 = operand_0 o2 c o3 [] in
                (
                    match op4 with
                    | None ->
                        let* f = make_binary b o2 e in
                        return (f, None)
                    | Some o4 ->
                        (* [o1 b o2 e o4] with [o2 > o4] *)
                        let* f = make_binary b o2 e in
                        (* [o1 f o4] *)
                        let* left = is_left o1 o4 in
                        if left then
                            return (f, Some o4)
                        else
                            operand_0 o1 f o4 []
                )

        and operand_3 o1 b o2 us u c o3 =
            (* o1 < o2 *)
            let* left = is_left u o3 in
            if left then
                let* e = make_unary u c in
                operand_1 o1 b o2 us e o3
            else
                let* e, op4 = operand_0 u c o3 [] in
                let _ = e, op4 in
                match op4 with
                | None ->
                    (* [o1 b o2 us u e] *)
                    let* f =
                        apply_unaries (u :: us) e
                        >>=
                        make_binary b o2
                    in
                    return (f, None)
                | Some o4 ->
                    (* [o1 b o2 us u e o4] with [o1 < o2] and [u > o4] *)
                    let* f = make_unary u e in
                    (* [o1 b o2 us f o4] *)
                    operand_1 o1 b o2 us f o4

        in
        expression_0 []
end (* Make *)
