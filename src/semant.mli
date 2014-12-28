type venv = Env.entry Symbol.Table.t

type tenv = Types.t Symbol.Table.t

type expty = {
  exp : Translate.exp;
  ty : Types.ty;
}

val trans_var : venv -> tenv -> Syntax.var -> expty

val trans_exp : venv -> tenv -> Syntax.exp -> expty

val trans_dec : venv -> tenv -> Syntax.dec -> venv * tenv

val trans_ty : tenv -> Syntax.ty -> Types.ty
