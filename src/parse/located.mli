(** A parsing construct located within a file. *)


type 'a t = Position.range * 'a
(** ['a t] Type of an object which is located within a file, i.e. which has a
   start and an end position. *)


val make: Position.range -> 'a -> 'a t
(** [make range object] Make [object] located in [range]. *)


val value: 'a t -> 'a
(** [value loc] The located object. *)


val start: 'a t -> Position.t
(** [start loc] The start position of the located object [loc]. *)


val _end: 'a t -> Position.t
(** [_end loc] The end position of the located object [loc]. *)


val range: 'a t -> Position.range
(** [range loc] The pair consisting of start position and end position of the
    located object [loc]. *)


val map: ('a -> 'b) -> 'a t -> 'b t
(** [map f loc] Map the value within the located object [loc] via the function
    [f].

    Equivalent to [make (range loc) (value loc |> f)].
*)
