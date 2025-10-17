type bit  = O | I
type bits = bit NonEmpty.t

let rec int_to_bits n : bits =
  if n = 0 then NonEmpty.Single O else
  if n = 1 then NonEmpty.Single I else
    let b = if n mod 2 = 0 then O else I in
    Cons(b, int_to_bits (n / 2))
