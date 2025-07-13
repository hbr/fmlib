(** Files *)
module type FILE =
sig

    (** A file handle that represents a user-selected file in the local
        filesystem. See {!Attribute.on_fileselect} on how to obtain file handles.
    *)

    type t

    val name: t -> string
    (** The filename. *)

    val media_type: t -> string option
    (** The media type, a.k.a. MIME type, or [None] if it is unknown. *)

    val size: t -> int
    (** The file size in bytes. *)

end
