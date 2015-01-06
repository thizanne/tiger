open Error
open Printf
open Location

module T = Types
module S = Syntax

type venv = Env.entry Symbol.Table.t

type tenv = Types.t Symbol.Table.t

type expty = {
  exp : Translate.exp;
  ty : Types.t;
}

let rec trans_exp venv (tenv : tenv) exp =
  let open Syntax in

  let rec check_ty ty exp =
    let { ty = ty'; _ } = trexp exp in
    if ty <> ty'
    then
      type_error exp.loc
      @@ sprintf "%s expected, found %s" (T.to_string ty) (T.to_string ty')

  and check_int exp = check_ty T.Int exp

  and check_unit exp = check_ty T.Unit exp

  and trexp exp =
    match exp.item with
    | Var v ->
      trvar v
    | Nil _ ->
      { exp = (); ty = T.Nil }
    | Int n ->
      { exp = (); ty = T.Int }
    | String s ->
      { exp = (); ty = T.String }
    | Op (op, x, y) ->
      check_int x;
      check_int y;
      { exp = (); ty = T.Int }
    | Seq exps ->
      List.fold_left
        (fun _ exp -> trexp exp)
        { exp = (); ty = T.Unit }
        exps
    | Assign (var, exp) ->
      let { ty = vty; _ } = trvar var in
      let { ty = ety; _ } = trexp exp in
      if vty = ety
      then { ty = vty; exp = () }
      else
        type_error exp.loc
        @@ sprintf "Trying to affect a %s to a %s variable"
          (T.to_string ety) (T.to_string vty)
    | If (cond, iftrue, iffalse) ->
      check_int cond;
      let { ty = tty; _ } = trexp iftrue in
      begin match iffalse with
        | None -> { ty = tty; exp = () }
        | Some iffalse ->
          let { ty = fty; _ } = trexp iffalse in
          if tty = fty
          then { ty = tty; exp = () }
          else
            type_error exp.loc
            @@ sprintf
              "Different types in the branches of a condition: %s and %s"
              (T.to_string tty) (T.to_string fty)
      end
    | While (cond, body) ->
      check_int cond;
      check_unit body;
      { ty = T.Unit; exp = () }
    | For (sym, esc, from, to_, body) ->
      check_int from;
      check_int to_;
      check_unit body;
      { ty = T.Unit; exp = () }
    | Break _ ->
      { ty = T.Unit; exp = () }
    | Let (decs, body) ->
      let venv, tenv = trans_decs venv tenv decs in
      trans_exp venv tenv body
    | Array (typ, size, init) ->
      check_int size;
      let { ty = ity; _ } = trexp init in
      let tty = Symbol.Table.find typ.item tenv in
      begin match tty with
        | T.Array (ty, _) ->
          if ity = ty then { ty = tty; exp = () }
          else
            type_error init.loc
            @@ sprintf
              "Wrong type for initial value of an array: found %s, expected %s"
              (T.to_string ity) (T.to_string ty)
        | _ -> type_error typ.loc @@ sprintf "Not an array type: %s" (T.to_string tty)
      end
    | Record (typ, ifields) ->
      let rty = Symbol.Table.find typ.item tenv in
      begin match rty with
        | T.Record (fields, _uniq) ->
          List.iter2
            (fun (sym, typ) (isym, iexp) ->
               if sym = isym.item
               then check_ty typ iexp
               else
                 name_error isym.loc
                 @@ sprintf "Wrong field %s: expected %s"
                   (Symbol.name isym.item) (Symbol.name sym))
            fields ifields;
          { exp = (); ty = rty }
        | _ -> type_error typ.loc @@ sprintf "Not a record type: %s" (T.to_string rty)
      end
    | Call (f, args) ->
      let fentry = Symbol.Table.find f.item venv in
      begin match fentry with
        | Env.VarEntry _ ->
          type_error f.loc
          @@ sprintf "%s is not a function, it cannot be applied"
            (Symbol.name f.item)
        | Env.FunEntry (formals, result) ->
          List.iter2 check_ty formals args;
          { ty = result; exp = () }
      end

  and trvar var = assert false

  in trexp exp

and trans_dec dec = assert false

and trans_decs venv tenv =
  List.fold_left
    (fun (venv, tenv) dec -> trans_dec venv tenv dec)
    (venv, tenv)

let trans_ty = assert false
