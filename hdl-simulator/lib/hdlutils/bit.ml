type t = bool

let to_int b =
  if b then 1 else 0

let nandb b1 b2 =
  if b1 && b2 then false
  else true
