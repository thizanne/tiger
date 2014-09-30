type t

val symbol : string -> t
val name : t -> string

module Table : Map.S with type key = t
