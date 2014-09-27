type t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
}

type 'a loc = {
  item : 'a;
  loc : t;
}

let mk startpos endpos =
  {startpos; endpos}

let mkloc item loc =
  {item; loc}
