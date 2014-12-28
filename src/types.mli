type unique

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

val actual : 'a t -> actual t
