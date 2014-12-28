type access

type ty

type entry =
  | VarEntry of ty
  | FunEntry of ty list * ty

val base_tenv : ty Symbol.Table.t
val base_venv : entry Symbol.Table.t
