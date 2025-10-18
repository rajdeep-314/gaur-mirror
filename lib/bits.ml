type bit  = O | I
type bits = bit NonEmpty.t

let rec int_to_bits n : bits =
  if n = 0 then NonEmpty.Single O else
  if n = 1 then NonEmpty.Single I else
    let b = if n mod 2 = 0 then O else I in
    Cons(b, int_to_bits (n / 2))
  
let bit_to_int = function
  | O -> 0
  | I -> 1

let rec bits_to_int (bits: bits) =
  match bits with 
  | Single b -> bit_to_int b
  | Cons (b, bs) -> 2 * (bits_to_int bs) + (bit_to_int b)
