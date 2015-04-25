open Printf

type t =
  | Int
  | String
  | Record of (Symbol.t * t) list * Unique.t
  | Array of t * Unique.t
  | Nil
  | Unit
  | Name of Symbol.t * t option ref

let rec actual = function
  | Name (sym, {contents = None}) -> failwith "actual"
  | Name (_, {contents = Some t}) -> actual t
  | Int
  | String
  | Record _
  | Array _
  | Nil
  | Unit
    as t -> t

let rec to_string = function
  | Int -> "Int"
  | String -> "String"
  | Nil -> "Nil"
  | Unit -> "Unit"
  | Name (sym, _) -> Symbol.name sym
  | Record (_, u) -> sprintf "Record %s" (Unique.to_string u)
  | Array (t, u) -> sprintf "Array %s %s" (to_string t) (Unique.to_string u)
