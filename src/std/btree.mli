(** Sets and maps based on B trees. *)

(** A finite set implemented by a B tree. *)
module Set (Key: Interfaces.SORTABLE):
sig
    (** {1 Set API} *)

    include Interfaces.SET with type item = Key.t


    val of_list: Key.t list -> t


    (** {1 Stream of elements} *)

    module Source:
    sig
        type set = t

        (** {1 Standard API} *)

        include Interfaces.SOURCE with type item = Key.t


        (** {1 Create a stream} *)

        val make: set -> t
    end
end


(** A finite map implemented by a B tree. *)
module Map (Key: Interfaces.SORTABLE):
sig
    (** {1 Map API} *)

    include Interfaces.MAP with type key = Key.t

    val of_list: (Key.t * 'a) list -> 'a t




    (** {1 Stream of key value pairs} *)

    (** All key value pairs of a finite map can be considered as a sorted list
        of key value pairs. It is possible to iterate over this sequence with
        the help of the function [fold_left]. However this function performs the
        whole iteration.

        Sometimes it is desirable to iterate over the sequence of the sorted key
        value pairs and keep the control over the iteration. For that purpose it
        is convenient to have the finite map as a stream of key value pairs.
    *)

    type 'a source
    (** Type of a stream of key value pairs. *)

    val make_source: 'a t -> 'a source
    (** Convert the map into a stream of key value pairs. *)


    val has_more: 'a source -> bool
    (** Has the stream of key value pairs more elements? *)

    val peek: 'a source -> Key.t * 'a
    (** The next key value pair of the stream. *)

    val advance: 'a source -> 'a source
    (** [advances source] Advance the stream by one element.

        Precondition: [has_more source]
    *)


    (** Module which satisfies the interface {!module-type: Interfaces.SOURCE} *)
    module Source (Value: Interfaces.ANY):
    sig
        type 'a map = 'a t


        (** {1 Standard API} *)

        include Interfaces.SOURCE with type item = Key.t * Value.t


        (** {1 Create a stream} *)

        val make: Value.t map -> t
    end
end
