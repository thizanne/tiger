type op =
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

type field = {
  name : Symbol.t Location.loc;
  escape : bool ref;
  typ : Symbol.t Location.loc;
}

type ty =
  | NameTy of Symbol.t Location.loc
  | RecordTy of field list
  | ArrayTy of Symbol.t Location.loc

type exp =
  | Var of var Location.loc
  | Nil of unit Location.loc
  | Int of int Location.loc
  | String of string Location.loc
  | Call of Symbol.t Location.loc * (* function name *)
            exp Location.loc list (* formals *)
  | Op of op Location.loc *
          exp Location.loc *
          exp Location.loc
  | Record of Symbol.t Location.loc * (* type name *)
              (Symbol.t Location.loc * exp Location.loc) list
  | Seq of exp Location.loc list
  | Assign of var Location.loc *
              exp Location.loc
  | If of exp Location.loc * (* condition *)
          exp Location.loc * (* then *)
          exp Location.loc option (* else *)
  | While of exp Location.loc * (* condition *)
             exp Location.loc (* body *)
  | For of Symbol.t * (* indice symbol *)
           bool ref * (* escapes *)
           exp Location.loc * (* from *)
           exp Location.loc * (* to *)
           exp Location.loc (* body *)
  | Break of unit Location.loc
  | Let of dec list *
           exp Location.loc
  | Array of Symbol.t Location.loc * (* type *)
             exp Location.loc * (* size *)
             exp Location.loc (* init *)
and var =
  | SimpleVar of Symbol.t Location.loc
  | FieldVar of var Location.loc *
                Symbol.t Location.loc
  | SubscriptVar of var Location.loc *
                    exp Location.loc

and dec =
  | FunctionDec of fundec Location.loc list
  | VarDec of vardec Location.loc
  | TypeDec of typedec Location.loc list

and fundec = {
  fun_name : Symbol.t Location.loc;
  params : field list;
  result_typ : Symbol.t Location.loc option;
  body : exp;
}

and vardec = {
  var_name : Symbol.t Location.loc;
  escape : bool ref;
  var_typ : Symbol.t Location.loc option;
  init : exp;
}

and typedec = {
  type_name : Symbol.t Location.loc;
  typ : ty;
}
