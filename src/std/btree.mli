(** Sets and maps based on B trees. *)

(** A finite set implemented by a B tree. *)
module Set (Key: Interfaces.SORTABLE):
sig
    (** {1 Set API} *)

    include Interfaces.SET with type item = Key.t


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



    (** {1 Stream of key value pairs} *)

    module Source (Value: Interfaces.ANY):
    sig
        type 'a map = 'a t


        (** {1 Standard API} *)

        include Interfaces.SOURCE with type item = Key.t * Value.t


        (** {1 Create a stream} *)

        val make: Value.t map -> t
    end
end
