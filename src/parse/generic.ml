open Std
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
        type t =
            | More  of B.t * (B.t -> t)
            | Done of B.t * Final.t option


        let needs_more (p: t): bool =
            match p with
            | More _ ->
                true
            | Done _ ->
                false


        let has_ended (p: t): bool =
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



        let state (p: t): State.t =
            match p with
            | More (b, _)
            | Done (b, _) ->
                B.state b


        let lookaheads (p: t): Token.t array * bool =
            match p with
            | More (b, _)
            | Done (b, _) ->
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


    let map (f: 'a -> 'b) (p: 'a t): 'b t =
        let* a = p in
        return (f a)



    let update (f: State.t -> State.t): unit t =
        fun b k ->
        k (Some ()) (B.update f b)


    let get: State.t t =
        fun b k ->
        k (Some (B.state b)) b


    let get_and_update (f: State.t -> State.t): State.t t =
        fun b k ->
        let st = B.state b in
        k (Some st) (B.update f b)




    (* Basic Combinators *)

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
                  assert (B.has_lookahead b);
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



    let make_parser (s: State.t) (p: Final.t t): Parser.t =
        p
            (B.init s)
            (fun res b -> Done (b, res))



    let make (state: State.t) (p: Final.t t) (e: State.t -> Expect.t)
        : Parser.t
        =
        make_parser
            state
            (p >>= expect_end e)





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


    let (<?>) (p: 'a t) (e: Expect.t): 'a t =
        fun b0 k ->
        p
            (B.start_alternatives b0)
            (fun res b ->
                 match res with
                 | None ->
                     k None (B.end_failed_alternatives e b0 b)
                 | Some a ->
                     k (Some a) (B.end_succeeded_alternatives b0 b))


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


    let zero_or_more (start: 'r) (f: 'item -> 'r -> 'r) (p: 'item t): 'r t =
        let rec many r =
            (
                let* a = consumer p in
                many (f a r)
            )
            </>
            return r
        in
        many start


    let one_or_more
            (first: 'item -> 'r)
            (next:  'item -> 'r -> 'r)
            (p: 'item t)
        : 'r t
        =
        let* a = p in
        zero_or_more (first a) next p


    let list_zero_or_more (p: 'a t): 'a list t =
        let* xs = zero_or_more [] (fun x xs -> x :: xs) p
        in
        return (List.rev xs)


    let list_one_or_more (p: 'a t): ('a * 'a list) t =
        let* x = p in
        let* xs = list_zero_or_more p in
        return (x, xs)


    let skip_zero_or_more (p: 'a t): int t =
        zero_or_more 0 (fun _ i -> i + 1) p


    let skip_one_or_more (p: 'a t): int t =
        let* n = skip_zero_or_more p in
        let* _ = p in
        return (n + 1)


    let one_or_more_separated
            (first: 'item -> 'r)
            (next:  'r -> 'sep  -> 'item -> 'r)
            (p: 'item t)
            (sep: 'sep t)
        : 'r t
        =
        let rec many r =
            (
                let* s    = sep in
                let* item = p in
                many (next r s item))
                </>
                return r
            in
            let* item = p in
            many (first item)

    (*
        let one_or_more_separated (p: 'a t) (sep: _ t): 'a list t =
            return (fun a l -> a :: l)
            |= p
            |= zero_or_more (sep >>= fun _ -> p)


        let zero_or_more_separated (p: 'a t) (sep: _ t): 'a list t =
            one_or_more_separated p sep
            </> return []*)
end (* Make *)
