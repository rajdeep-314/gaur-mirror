type t = Bit.t NonEmpty.t

let rec of_int n =
  let open Bit in
  if n = 0 then NonEmpty.Single true  else
  if n = 1 then NonEmpty.Single false else
    let b = if n mod 2 = 0 then false else true in
    Cons(b, of_int (n / 2))

let rec to_int bits =
  match bits with
  | NonEmpty.Single b -> Bit.to_int b
  | NonEmpty.Cons (b, bs) -> 2 * (to_int bs) + (Bit.to_int b)
