type t = int

let symbol, name =
  let n = ref (-1) in
  let table = Hashtbl.create 128 in

  (function name ->
    incr n;
    Hashtbl.add table !n name;
    !n),

  (function symbol ->
    Hashtbl.find table symbol)

module Ord = struct
  type symbol = t
  type t = symbol

  let compare = Pervasives.compare
end

module Table = Map.Make (Ord)
