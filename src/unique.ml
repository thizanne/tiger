open Printf

type t = int

let create =
  let n = ref (-1) in
  function () ->
    incr n;
    !n

let to_string =
  sprintf "#%d"
