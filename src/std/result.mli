(** Result: Handling results of operations which can fail *)


(** {1 Overview} *)

(**

    Operations returning a result type can be used to have some {i functional
    exception handling}.

    Let's say that you have some operatios returning a result object.

    {[
        let op1 ...  : (int,    error) result  = ...
        let op2 ...  : (char,   error) result = ...
        let op3 ...  : (string, error) result = ...
        let op3 ...  : (t,      error) result = ...
    ]}

    You can chain these operations by concentrating on the success case only and
    handling the error case at the end of the chain.

    {[
        match
            let* i = op1 ...  in
            let* c = op2 ... i ...  in
            let* s = op3 ... i ... c ...  in
            op4 ... i ... c ... s
        with
        | Ok x ->
            (* Handling of the success case *)
        | Error e ->
            (* Handling of the error case which might have
               occurred in any of the steps *)
    ]}


    A simple example:
    {[
        type 'a r = ('a, string) result

        let add (a: int r) (b: int r): int r =
            let* x = a in
            let* y = b in
            Ok (x + y)

        let divide (a: int r) (b: int r): int r =
            let* x = a in
            let* y = b in
            if y = 0 then
                Error "Division by Zero"
            else
                Ok (x / y)

        assert (
            add (Ok 1) (divide (Ok 2) (Ok 0))
            =
            Error "Division by Zero"
        )

        assert (
            add (Ok 1) (divide (Ok 10) (Ok 2))
            =
            Ok 6
        )
    ]}

*)



(** {1 API} *)

type ('a,'e) t = ('a, 'e) result
(** ['a] is the result type in case of success and ['e] is the result type in
    case of failure. It is implemented by the ocaml type [result] from the ocaml
    standard library.
*)


val return: 'a -> ('a, 'e) t
(** [return a] Equivalent to [Ok a]. *)


val fail: 'e -> ('a, 'e) t
(** [fail e] Equivalent to [Error e]. *)


val to_option: ('a, 'e) t -> 'a option
(** [to_option r] Map [r] to an optional element i.e. [Some a] in case of [Ok a]
    and [None] in case of [Error _]. *)


val (>>=): ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
(** [m >>= f]

    maps success result [m] to [f a]. In case of an error result [f]
    is not called and the error remains.
*)


val ( let* ): ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
(**

   [let* a = m in f a] is the same as [m >>= f]
*)



val map: ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
(** [map f m] Map the result in [m] via the function [f]. *)



val map_error: ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t
(** [map_error f m] Map the error in [m] via the function [f]. *)



val get: ('a, Void.t) t -> 'a
(** [get m] Get the ok content of a result object which cannot have errors. *)





(** {1 Monad} *)

(** The result type encapsulated in a module which satisfies the monadic
    interface. *)
module Monad (E: Interfaces.ANY):
sig
    type 'a t = ('a, E.t) result


    val return: 'a -> 'a t

    val fail: E.t -> 'a t

    val to_option: 'a t -> 'a option

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t


end
