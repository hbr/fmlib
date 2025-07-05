(** The javascript file object *)

(**
    The javascript file object represents a user-selected file in the local
    filesystem. For example, when the user clicks on an [<input type="file">]
    element and the [changed] event is fired, the [files] property of that
    element can be decoded into a list of files using the
    {!Base.Decode.file_list} decoder.
  *)

type t


val name: t -> string
(** The filename. *)


val media_type: t -> string option
(** The media type, a.k.a. MIME type, or [None] if it is unknown. *)


val size: t -> int
(** The file size in bytes. *)
