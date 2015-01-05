type unique = unit ref

type t =
  | Int
  | String
  | Record of (Symbol.t * t) list * unique
  | Array of t * unique
  | Nil
  | Unit
  | Name of Symbol.t * t option ref

let rec actual = function
  | Name (sym, {contents = None}) -> assert false
  | Name (_, {contents = Some t}) -> actual t
  | Int
  | String
  | Record _
  | Array _
  | Nil
  | Unit
    as t -> t

let to_string = function
  | Int -> "Int"
  | String -> "String"
  | Record _ -> "Record"
  | Array _ -> "Array"
  | Nil -> "Nil"
  | Unit -> "Unit"
  | Name (sym, _) -> Symbol.name sym
