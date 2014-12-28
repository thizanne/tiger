type unique = unit ref

type actual

type _ t =
  | Int : actual t
  | String : actual t
  | Record : (Symbol.t * 'a t) list * unique -> actual t
  | Array : 'a t * unique -> actual t
  | Nil : actual t
  | Unit : actual t
  | Name : Symbol.t * 'a t option ref -> 'b t

type actual_type = actual t

let rec actual : type a. a t -> actual t = function ty ->
  match ty with
  | Name (sym, {contents = None}) -> assert false
  | Name (_, {contents = Some t}) -> actual t
  | Int -> ty
  | String -> ty
  | Record _ -> ty
  | Array _ -> ty
  | Nil -> ty
  | Unit -> ty
