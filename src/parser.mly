%{
 open Lexing
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

%start <unit> prog

%%

%inline loc(X) :
| x = X { let open Location in mkloc x (mk $startpos $endpos) }

prog :
| exp Eof { }
| error { Error.syntax_error (Location.mk $startpos $endpos) "" }

exp :
| lvalue { }
| Nil { }
| LPar separated_list(Semi, exp) RPar { }
| Int { }
| Minus exp { } %prec UnaryMinus
| exp op exp { }
| String { }
| Id LPar separated_list(Comma, exp) RPar { }
| Id LBrace separated_list(Comma, field_assign) RBrace { }
| Id LBracket exp RBracket Of exp { } %prec LBracket
| lvalue ColonEq exp { }
| If exp Then exp { } %prec Else
| If exp Then exp Else exp { }
| While exp Do exp { } %prec loop
| For Id ColonEq exp To exp Do exp { } %prec loop
| Break { }
| Let decs In separated_list(Semi, exp) End { }

lvalue :
| Id { } %prec LBracket
| lvalue Dot Id { }
| Id LBracket exp RBracket { } (* Redundant but needed to solve a conflict *)
| lvalue LBracket exp RBracket { }

%inline op :
| Plus {  }
| Minus { }
| Times { }
| Divide { }
| Eq { }
| Neq { }
| Gt { }
| Ge { }
| Lt { }
| Le { }
| Ampersand { }
| Pipe { }

field_assign :
| Id Eq exp { }

decs :
| l = list(dec) { l }

dec :
| tydec { }
| vardec { }
| fundec { }

tydec :
| Type Id Eq ty { }

ty :
| Id { }
| LBrace tyfields RBrace { }
| Array Of Id { }

tyfields :
| separated_list(Comma, tyfield) { }

tyfield :
| Id Colon Id { }

vardec :
| Var Id ColonEq exp { }
| Var Id Colon Id ColonEq exp { }

fundec :
| Function Id LPar tyfields RPar Eq exp { }
| Function Id LPar tyfields RPar Colon Id Eq exp { }
