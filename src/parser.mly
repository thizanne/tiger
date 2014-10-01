%{
  open Lexing
  module S = Syntax
  module L = Location
%}

%token While For To Break Let In End Function Var
%token Type Array If Then Else Do Of Nil
%token Comma Colon Semi Dot
%token LPar RPar LBracket RBracket LBrace RBrace
%token Plus Minus Times Divide
%token Eq Neq Lt Le Gt Ge
%token Ampersand Pipe ColonEq
%token Eof

%token <int> Int
%token <string> String
%token <string> Id

%right Function Type
%nonassoc loop
%right Else
%nonassoc ColonEq
%right LBracket
%left Pipe
%left Ampersand
%nonassoc Eq Neq Gt Ge Lt Le
%left Plus Minus
%left Times Divide
%left UnaryMinus

%start <Syntax.exp> prog

%%

%inline loc(X) :
| x = X { L.mkloc x (L.mk $startpos $endpos) }

symbol :
| x = loc(Id) { L.mkloc (Symbol.symbol x.L.item) x.L.loc }

prog :
| e = exp Eof { e }
| error { Error.syntax_error (L.mk $startpos $endpos) "" }

exp :
| var = loc(lvalue) { S.Var var }
| unit = loc(Nil) { S.Nil unit }
| LPar seq = separated_list(Semi, loc(exp)) RPar { S.Seq seq }
| n = loc(Int) { S.Int n }
| _minus = Minus e = loc(exp) {
    S.Op (
      L.mkloc S.Minus (L.mk $startpos(_minus) $endpos(_minus)),
      L.mkdummy (S.Int (L.mkdummy 0)),
      e
    )
  } %prec UnaryMinus
| e1 = loc(exp) o = loc(op) e2 = loc(exp) {
    S.Op (o, e1, e2)
  }
| e1 = loc(exp) Ampersand e2 = loc(exp) {
    S.If (
      e1,
      e2,
      Some (L.mkdummy (S.Int (L.mkdummy 0)))
    )
  }
| e1 = loc(exp) Pipe e2 = loc(exp) {
    S.If (
      e1,
      L.mkdummy (S.Int (L.mkdummy 1)),
      Some e2
    )
  }
| s = loc(String) { S.String s }
| fn = symbol LPar formals = separated_list(Comma, loc(exp)) RPar {
    S.Call (
      fn,
      formals
    )
  }
| typ = symbol LBrace fields = separated_list(Comma, field_assign) RBrace {
    S.Record (
      typ,
      fields
    )
  }
| typ = symbol LBracket size = loc(exp) RBracket Of init = loc(exp) {
    S.Array (
      typ,
      size,
      init
    )
  } %prec LBracket
| var = loc(lvalue) ColonEq exp = loc(exp) {
    S.Assign (
      var,
      exp
    )
  }
| If cond = loc(exp) Then if_true = loc(exp) {
    S.If (
      cond,
      if_true,
      None
    )
  } %prec Else
| If cond = loc(exp) Then if_true = loc(exp) Else if_false = loc(exp) {
    S.If (
      cond,
      if_true,
      Some if_false
    )
  }
| While cond = loc(exp) Do body = loc(exp) {
    S.While (
      cond,
      body
    )
  } %prec loop
| For i = symbol ColonEq from = loc(exp) To to_ = loc(exp) Do body = loc(exp) {
    S.For (
      i.L.item,
      ref true,
      from,
      to_,
      body
    )
  } %prec loop
| unit = loc(Break) { S.Break unit }
| Let decs = decs In body = loc(separated_list(Semi, loc(exp))) End {
    S.Let (
      decs,
      L.mkloc (S.Seq body.L.item) body.L.loc
    )
  }

lvalue :
| x = symbol { S.SimpleVar x } %prec LBracket
| var = loc(lvalue) Dot field = symbol {
    S.FieldVar (
      var,
      field
    )
  }
| var = symbol LBracket subscript = loc(exp) RBracket {
    (* Redundant but needed to solve a conflict *)
    S.SubscriptVar (
      L.mkloc (S.SimpleVar var) var.L.loc,
      subscript
    )
  }
| var = loc(lvalue) LBracket subscript = loc(exp) RBracket {
    S.SubscriptVar (
      var,
      subscript
    )
  }

%inline op :
| Plus { S.Plus }
| Minus { S.Minus }
| Times { S.Times }
| Divide { S.Divide }
| Eq { S.Eq }
| Neq { S.Neq }
| Gt { S.Gt }
| Ge { S.Ge }
| Lt { S.Lt }
| Le { S.Le }

field_assign :
| name = symbol Eq exp = loc(exp) { (name, exp) }

decs :
| l = dec* { l }

dec :
| t = loc(tydec)+ { S.TypeDec t }
| v = loc(vardec) { S.VarDec v }
| f = loc(fundec)+ { S.FunctionDec f }

%inline tydec :
| Type type_name = symbol Eq typ = ty { S.{ type_name; typ } }

ty :
| t = symbol { S.NameTy t }
| LBrace fields = tyfields RBrace { S.RecordTy fields }
| Array Of ty = symbol { S.ArrayTy ty }

tyfields :
| fields = separated_list(Comma, tyfield) { fields }

tyfield :
| name = symbol Colon typ = symbol { S.{ name; typ; escape = ref true  } }

vardec :
| Var var_name = symbol ColonEq init = exp {
    S.{
      var_name;
      escape = ref true;
      var_typ = None;
      init;
    }
  }
| Var var_name = symbol Colon var_typ = symbol ColonEq init = exp {
    S.{
      var_name;
      escape = ref true;
      var_typ = Some var_typ;
      init;
    }
  }

%inline fundec :
| Function fun_name = symbol LPar params = tyfields RPar Eq body = exp {
    S.{
      fun_name;
      params;
      result_typ = None;
      body
    }
  }
| Function fun_name = symbol LPar params = tyfields RPar
    Colon result_typ = symbol Eq body = exp {
    S.{
      fun_name;
      params;
      result_typ = Some result_typ;
      body
    }
  }
