type venv = Env.entry Symbol.Table.t

type tenv = Types.t Symbol.Table.t

type expty = {
  exp : Translate.exp;
  ty : Types.t;
}

val trans_exp : venv -> tenv -> Syntax.exp Location.loc -> expty

val trans_dec : venv -> tenv -> Syntax.dec -> venv * tenv

val trans_ty : tenv -> Syntax.ty -> Types.t
