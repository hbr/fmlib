(** Encoders and Decoders for Unicode Characters encoded in UTF-16. *)



(** Encoder and Decoder for Unicode Characters encoded in UTF-16 Big Endian. *)
module Be:
sig
    include Interfaces.CHAR_CODEC (** @inline *)
end



(** Encoder and Decoder for Unicode Characters encoded in UTF-16 Little Endian.
 *)
module Le:
sig
    include Interfaces.CHAR_CODEC (** @inline *)
end
