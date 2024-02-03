open Interfaces



module type ANY = Fmlib_std.Interfaces.ANY

module String = Fmlib_std.String




module Make
        (Codec: CHAR_CODEC)
        (User:  ANY)
        (Final: ANY)
        (Semantic: ANY)
=
struct
    module State = Character_state.Make (User)

    module Dec = Codec.Decoder

    module Enc = Codec.Encoder



    module Expect =
    struct
        type t = string * Indent.violation option
    end


    module Basic =
        Generic.Make
            (Dec)
            (State)
            (Expect)
            (Semantic)
            (Final)

    include  Basic


    module Parser =
    struct
        module P = Basic.Parser
        type token    = Dec.t
        type item     = token
        type final    = Final.t
        type state    = User.t
        type expect   = string * Indent.violation option
        type semantic = Semantic.t

        type t = P.t

        let needs_more          = P.needs_more
        let has_ended           = P.has_ended
        let has_received_end    = P.has_received_end
        let has_consumed_end    = P.has_consumed_end
        let has_succeeded       = P.has_succeeded
        let has_failed_syntax   = P.has_failed_syntax
        let has_failed_semantic = P.has_failed_semantic
        let final               = P.final
        let failed_expectations = P.failed_expectations
        let failed_semantic     = P.failed_semantic
        let has_lookahead       = P.has_lookahead
        let first_lookahead_token = P.first_lookahead_token
        let fold_lookahead      = P.fold_lookahead
        let transfer_lookahead  = P.transfer_lookahead
        let lookaheads          = P.lookaheads

        let put                 = P.put
        let put_end             = P.put_end


        let position (p: t): Position.t =
            P.state p |> State.position


        let line (p: t): int =
            P.state p |> State.line


        let column (p: t): int =
            P.state p |> State.column


        let byte_column (p: t): int =
            P.state p |> State.byte_column


        let state (p: t): User.t =
            P.state p |> State.user

        module Run = Run_on.Make (Dec)

        let run_on_string    = Run.string    needs_more put put_end

        let run_on_string_at = Run.string_at needs_more put put_end

        let run_on_channel   = Run.channel   needs_more put put_end
    end








    (* General Combinators
       ============================================================
     *)




    let expect_error (e: string) (_: State.t): Expect.t =
        e, None
    (*e, State.indent st*)


    let unexpected (e: string): 'a t =
        Basic.( unexpected (e, None) )


    let (<?>) (p: 'a t) (e: string): 'a t =
        Basic.(
            update_expectations
                (fun state -> function
                     | None ->
                         (* end of input reached *)
                         (e, None)
                     | Some _ ->
                         (e, State.check_position state)
                )
                p
        )


    let map_and_update (f: User.t -> 'a -> 'b * User.t) (p: 'a t): 'b t =
        Basic.(
            map_and_update
                (fun state a ->
                     let b, user = f (State.user state) a
                     in
                     b,
                     State.put user state)
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



    let backtrack (p: 'a t) (e: string): 'a t =
        Basic.( backtrack p (e, None) )


    let followed_by (p: 'a t) (e: string): 'a t =
        Basic.( followed_by p (e, None) )


    let not_followed_by (p: 'a t) (e: string): unit t =
        Basic.( not_followed_by p (e, None) )


    let expect_end (a: 'a): 'a t =
        Basic.expect_end (fun _ -> "end of input", None) a



    let position: Position.t t =
        Basic.(map State.position get)



    let located (p: 'a t): 'a Located.t t =
        let* state1 = Basic.get in
        let* a      = p in
        let* state2 = Basic.get in
        return
            (Located.make
                 (State.position state1,
                  State.position state2)
                 a)







    (* Indentation Combinators
       ============================================================
     *)


    let indent (i: int) (p: 'a t): 'a t =
        assert (0 <= i);
        let* state = Basic.get_and_update (State.start_indent i) in
        let* a     = p in
        let* _     = Basic.update (State.end_indent i state) in
        return a


    let align0 (left: bool) (p: 'a t): 'a t =
        let f_align =
            if left then
                State.left_align
            else
                State.align
        in
        Basic.state_around
            f_align
            p
            (fun s0 _ s1 -> State.end_align s0 s1)


    let align (p:'a t): 'a t =
        align0 false p


    let left_align (p:'a t): 'a t =
        align0 true p


    let detach (p: 'a t): 'a t =
        let* state = Basic.get_and_update State.start_detach in
        let* a     = p in
        let* _     = Basic.update (State.end_detach state) in
        return a







    (* Character Combinators
       ============================================================
     *)
    let step
        (f: State.t -> Uchar.t -> ('a, string) result)
        (e: string)
        :
        'a t
        =
        (* Auxiliary function to get a combinator receiving one character.

           The function [f] decides what to do with a character received in a
           certain state.

           [e] is needed to generate an expect message in case that
           we are offside or at the end of input.
        *)
        Basic.step
            (fun state -> function
                | None ->
                    (* end of input reached. *)
                    Error (e, None)

                | Some c -> begin
                        match State.check_position state with
                        | None ->
                            (match f state (Dec.uchar c) with
                             | Ok a ->
                                 let state =
                                     if Dec.is_newline c then
                                         State.newline (Dec.byte_width c) state
                                     else
                                         State.advance
                                             (Dec.byte_width c)
                                             (Dec.width c)
                                             state
                                 in
                                 Ok (a, state)

                             | Error e ->
                                 Error (e, None)
                            )
                        | Some vio ->
                            Error (e, Some vio)
                    end
            )


    let uchar (expected: Uchar.t): Uchar.t t =
        let error () =
            Printf.sprintf
                "\"%s\" (U+%X)"
                (Enc.to_external expected)
                (Uchar.to_int expected)
        in
        step
            (fun _ actual ->
                 if expected = actual then
                     Ok expected
                 else
                     Error (error ())
            )
            (error ())



    let char (expected: char): char t =
        assert (Char.code expected < 128);
        let* _ = uchar (Uchar.of_char expected) in
        return expected



    let ucharp (f: Uchar.t -> bool) (e: string): Uchar.t t =
        step
            (fun _ c ->
                 if f c then
                     Ok c
                 else
                     Error e)
            e


    let charp (f: char -> bool) (e: string): char t =
        map
            Uchar.to_char
            (ucharp
                (fun uc ->
                     let i = Uchar.to_int uc in
                     0 <= i && i < 128 && f (Char.chr i))
                e
            )


    let range (c1: char) (c2: char): char t =
        charp
            (fun c -> c1 <= c && c <= c2)
            (Printf.sprintf "character between %c and %c" c1 c2)


    let urange (c1: Uchar.t) (c2: Uchar.t): Uchar.t t =
        let i1 = Uchar.to_int c1
        and i2 = Uchar.to_int c2
        in
        ucharp
            (fun c ->
                 let i = Uchar.to_int c in
                 i1 <= i && i <= i2
            )
            (Printf.sprintf
                 "unicode character between U+%X and U+%X"
                 i1
                 i2
            )


    let uppercase_letter: char t =
        charp
            (fun c -> 'A' <= c && c <= 'Z')
            "uppercase letter"


    let lowercase_letter: char t =
        charp
            (fun c -> 'a' <= c && c <= 'z')
            "lower case letter"


    let letter: char t =
        charp
            (fun c ->
                 ('A' <= c && c <= 'Z')
                 ||
                 ('a' <= c && c <= 'z'))
            "letter"


    let digit_char: char t =
        charp (fun c -> '0' <= c && c <= '9') "digit"


    let digit: int t =
        let* d = digit_char
        in
        return Char.(code d - code '0')

    let word
            (first: char -> bool)
            (inner: char -> bool)
            (expect: string)
        : string t
        =
        let* c0 = charp first expect in
        zero_or_more_fold_left
            (String.make 1 c0)
            (fun str c -> str ^ String.make 1 c |> return)
            (charp inner expect)
        |> no_expectations


    let hex_lowercase: int t =
        let* c = range 'a' 'f' in
        return Char.(code c - code 'a' + 10)


    let hex_uppercase: int t =
        let* c = range 'A' 'F' in
        return Char.(code c - code 'A' + 10)

    let hex_digit: int t =
        digit </> hex_lowercase </> hex_uppercase <?> "hex digit"



    let string (str: string): string t =
        let len = String.length str in
        let rec parse i =
            if i = len then
                return str
            else
                let* _ = char str.[i] in
                parse (i + 1)
        in
        parse 0


    let one_of_chars (str:string) (e: string): char t =
        let p c = String.has (fun d -> c = d)  0 str
        in
        charp p e




    let uword
            (first: Uchar.t -> bool)
            (inner: Uchar.t -> bool)
            (expect: string)
        : string t
        =
        let* c0 = ucharp first expect in
        zero_or_more_fold_left
            (Enc.to_internal c0)
            (fun str c -> str ^ Enc.to_internal c |> return)
            (ucharp inner expect)
        |> no_expectations









    (* Lexer support
       ============================================================
    *)

    let lexer
            (ws:        'a t)
            (end_token: 'tok)
            (tok:      'tok t)
        : (Position.range * 'tok) t
        =
        let* _ = ws in
        located (
            tok
            </>
            expect_end end_token
        )





    (* Make the Final Parser
       ============================================================
     *)

    let make (user: User.t) (p: Final.t t): Parser.t =
        Basic.make
            (State.make Position.start user)
            p
            (expect_error "end of input")


    let make_partial
            (pos: Position.t)
            (user: User.t)
            (p: Final.t t)
        : Parser.t
        =
        Basic.make_partial
            (State.make pos user)
            p
end





module Make_utf8
        (User:  ANY)
        (Final: ANY)
        (Semantic: ANY)
    =
    Make (Utf8) (User) (Final) (Semantic)





module Make_utf16_be
        (User:  ANY)
        (Final: ANY)
        (Semantic: ANY)
    =
    Make (Utf16.Be) (User) (Final) (Semantic)





module Make_utf16_le
        (User:  ANY)
        (Final: ANY)
        (Semantic: ANY)
    =
    Make (Utf16.Le) (User) (Final) (Semantic)
