type unique

type t =
  | Int
  | String
  | Record of (Symbol.t * t) list * unique
  | Array of t * unique
  | Nil
  | Unit
  | Name of Symbol.t * t option ref

val actual : t -> t

val to_string : t -> string
