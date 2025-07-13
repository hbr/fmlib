module type  RANDOM =
sig
    (** Generate Random Numbers *)


    type 'a t
    (** Generator, generating random values of type ['a]. *)


    val constant: 'a -> 'a t
    (** [constant a] Generate the same value every time. *)

    val (>>=):    'a t -> ('a -> 'b t) -> 'b t
    (** [rand >>= f] Generate the random value [a] using the generator [rand],
        and then use [f a] to generate a random value [b].
     *)


    val ( let* ): 'a t -> ('a -> 'b t) -> 'b t
    (** [let* a = rand in f a] is the same as [rand >>= f]. *)



    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f rand] Use [rand] to generate a random value and then map it by
        [f]. *)



    val int: int -> int t
    (** [int bound] A random generator which generates numbers [n] satisfying
        [0 <= n < bound].

        Precondition: [0 < bound]
     *)

    val float: float -> float t
    (** [float bound] A random generator which generates numbers [n] satisfying
        [0.0 <= n <= bound].

        Precondition: [0 <= bound]
     *)

    val bool: bool t
    (** Generate a random boolean value. *)

    val choose: 'a list -> 'a t
    (** [uniform lst] Generate a random value of the list [lst].

        Precondition: List must not be empty [lst <> []]
    *)
end
