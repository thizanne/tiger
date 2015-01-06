module T = Types
module S = Symbol

type access

type entry =
  | VarEntry of T.t
  | FunEntry of T.t list * T.t

let base_venv =
  S.Table.empty

let base_tenv =
  S.Table.empty
  |> S.Table.add (S.symbol "int") T.Int
  |> S.Table.add (S.symbol "string") T.String
