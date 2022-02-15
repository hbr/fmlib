open Fmlib_std
open Interfaces


module Make
        (State: ANY)
        (Token: ANY)
        (Expect: ANY)
        (Semantic: ANY)
=
struct
    module Error = Error.Make (Expect) (Semantic)

    type _end =
        | No_end
        | End_received
        | End_consumed


    type t = {
        state:
            State.t;

        has_consumed:
            (* Are there consumed token? *)
            bool;

        error: Error.t;

        la_ptr:
            (* Pointer to the first lookahead token in the buffer. *)
            int;

        is_buffering:
            (* Are we in buffering mode? I.e. are we within a backtrackable
               parser? *)
            bool;

        toks:
            (* Buffered token. The token from [la_ptr] to the end are lookahead
               token. *)
            Token.t array;

        _end:
            (* Buffered end of token stream. The end of token stream can only be
               consumed once. Once in the state [End_consumed] there is no way
               back. *)
            _end;
      }


    let init (st: State.t): t =
      {state =
           st;
       has_consumed =
           false;
       error =
           Error.init;
       la_ptr =
           0;
       is_buffering =
           false;
       toks =
           [||];
       _end =
           No_end}


    let state (b: t): State.t =
      b.state


    let error (b: t): Error.t =
      b.error


    let count_toks (b: t): int =
        (* Number of token in the buffer. *)
        Array.length b.toks


    let has_end (b: t): bool =
        match b._end with
        | No_end ->
            false
        | _ ->
            true


    let has_lookahead (b: t): bool =
        (* Are there lookahead token in the buffer? *)
        b.la_ptr < count_toks b || b._end = End_received


    let lookaheads (b: t): Token.t array =
        (* An array consisting only of the lookahead token in the buffer. *)
        let len = count_toks b - b.la_ptr
        in
        Array.sub b.toks b.la_ptr len


    let first_lookahead (b: t): Token.t option =
        (* The first lookahead token. *)
        assert (has_lookahead b);
        if b.la_ptr < count_toks b then
            Some b.toks.(b.la_ptr)
        else
            None


    let push_token (t: Token.t) (b: t): t =
        (* Push a new lookahead token to the buffer. *)
        if b.is_buffering || has_lookahead b then
            (* In buffering mode or if they are lookahead token, the new token
               is pushed to the buffer. *)
            {b with
             toks =
                 Array.push t b.toks}

        else
            (* Not in buffering mode an no lookaheads. We can forget all token
               in the buffer. *)
            {b with
             la_ptr =
                 0;
             toks =
                 [|t|]}


    let push_end (b: t): t =
        assert (not (has_end b));
        {b with _end = End_received}


    let put (state: State.t) (b: t): t =
        {b with state}


    let update (f: State.t -> State.t) (b: t): t =
        (* Update the state. *)
        {b with state = f b.state}


    let add_expected (e: Expect.t) (b: t): t =
        {b with error = Error.add_expected e b.error}


    let put_error (e: Semantic.t) (b: t): t =
        {b with error = Error.make_semantic e}


    let clear_errors  (b: t): t =
        {b with error = Error.init}

    let clear_last_error (b: t): t =
        {b with error = Error.clear_last b.error}


    let reset_errors (b0: t) (b: t): t =
        {b with error = b0.error}


    let consume (state: State.t) (b: t): t =
        (* Consume the first lookahead token. *)
        assert (has_lookahead b);
        if b.la_ptr < count_toks b then
            {b with
             state;
             has_consumed = true;
             error = Error.init;
             la_ptr = 1 + b.la_ptr}

        else if b._end = End_received then
            {b with
             state;
             has_consumed = true;
             error = Error.init;
             _end  = End_consumed}

        else
            assert false (* Cannot happen. *)



    let reject (e: Expect.t) (b: t): t =
        (* Reject the first lookahead token.

           The token are unchanged. The failed expectation [e] is added to the
           syntax errors.
        *)
        add_expected e b


    let start_new_consumer (b: t): t =
      {b with has_consumed = false}


    let has_consumed (b: t): bool =
      b.has_consumed


    let end_new_consumer (b0: t) (b: t): t =
        {b with
            has_consumed =
                b0.has_consumed || b.has_consumed;

            state =
                if b.has_consumed then
                    b.state
                else
                    b0.state
        }


    let start_alternatives (b: t): t =
        {b with has_consumed = false}


    let end_failed_alternatives (e: Expect.t) (b0: t) (b: t): t =
      if b.has_consumed then
          b
      else
          {b with
           has_consumed =
               b0.has_consumed;
           error =
               Error.add_expected e b0.error}


    let end_succeeded_alternatives (b0: t) (b: t): t =
        if b.has_consumed then
            b
        else
            {b with
             has_consumed =
                 b0.has_consumed;
             error =
                 b0.error}


    let start_backtrack (b: t): t =
        (* Start backtracking i.e. set the buffer into buffering mode.

           Token have to be buffered from now on. In case of failure we treat
           the consumed token as lookahead token.
        *)
        {b with is_buffering = true}


    let end_backtrack_success (b0: t) (b: t): t =
        (* The current backtrackable parser has succeeded. *)
        if b0.is_buffering then
            (* The current backtrackable parser is nested within another
               backtrackable parser. Therefore no change to the buffer. *)
            b

        else
            (* The current backtrackable parser is not nested within another
               backtrackable parser. We end buffering and forget all consumed
               token. The lookahead tokens remain in the buffer. *)
            {b with
             is_buffering =
                 false;
             toks =
                 lookaheads b; (* only lookahead token *)
             la_ptr =
                 0}


    let end_backtrack_fail (e: Expect.t option) (b0: t) (b: t): t =
        (* The current backtrackable parser has failed.

           Reestablish the buffer at the start of the backtrackable parser and
           treat the consumed token as lookahead token (i.e. unconsume them).
        *)
        assert (count_toks b0 <= count_toks b);
        assert (b._end <> End_consumed);
        if b0.la_ptr = b.la_ptr then
            (* failed without consumption, no backtracking necessary *)
            {b with is_buffering = b0.is_buffering}
        else
            (* failed with consumption, backtracking necessary *)
            {
                b0 with
                toks  = b.toks;
                _end  = b._end;
                error =
                    match e with
                    | None -> b0.error (* not_followed_by ? *)
                    | Some e -> Error.add_expected e b0.error
            }
end
