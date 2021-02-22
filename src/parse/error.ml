(*
    A parse error is either a list of syntax error or on semantic error.

    We need a list of syntax errors for the following reason:

    A syntax error signifies that something has been expected and has not been
    encountered. Since we have alternatives which can all fail without consuming
    tokens we can have a list of failed expectations.
*)
open Fmlib_std
open Interfaces


module Make (Expect: ANY) (Semantic: ANY) =
struct
    type t =
        | Syntax of Expect.t list
        | Semantic of Semantic.t


    let to_string
            (e: t)
            (f: Expect.t -> string)
            (g: Semantic.t -> string)
        : string =
        match e with
        | Syntax lst ->
            "["
            ^ String.concat
                ", "
                (List.rev_map f lst)
            ^ "]"
        | Semantic sem ->
            g sem


    let init: t =
        Syntax []


    let clear_last (e: t): t =
        match e with
        | Syntax (_ :: tail) ->
            Syntax tail
        | _ ->
            e


    let add_expected (exp: Expect.t) (e: t): t =
        match e with
        | Syntax lst ->
            Syntax (exp :: lst)
        | _ ->
            Syntax [exp]


    let make_semantic (sem: Semantic.t): t =
        Semantic sem


    let make_expectations (lst: Expect.t list): t =
        Syntax (List.rev lst)


    let is_semantic (e: t): bool =
        match e with
        | Syntax _ -> false
        | _        -> true


    let is_syntax (e: t): bool =
        not (is_semantic e)



    let semantic (e: t): Semantic.t =
        match e with
        | Syntax _ ->
            assert false (* Illegal call! *)
        | Semantic sem ->
            sem


    let expectations (e: t): Expect.t list =
        match e with
        | Syntax es ->
            List.rev es
        | Semantic _ ->
            assert false (* Illegal call! *)
end
