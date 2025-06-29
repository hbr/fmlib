type t


val name: t -> string


val media_type: t -> string option


val size: t -> int




module List: sig

  type file := t


  type t


  val of_value: Base.Value.t -> t option


  val length: t -> int


  val item: int -> t -> file option

end
