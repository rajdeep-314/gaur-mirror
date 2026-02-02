type t = O | I

let to_int b =
  match b with
  | O -> 0
  | I -> 1

let nandb b1 b2 =
  match b1, b2 with
  | I, I -> O
  | _    -> I
