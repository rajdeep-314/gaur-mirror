type t = O | I

let to_bool b =
  match b with
  | O -> false
  | I -> true

let of_bool b =
  match b with
  | false -> O
  | true -> I

let nand (b1, b2) =
  match (b1, b2) with
  | I, I -> O
  | _ -> I
