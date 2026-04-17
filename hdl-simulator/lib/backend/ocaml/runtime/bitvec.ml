type bit = bool

type bv  = bit array

let append = Array.append

let ( ++ ) = append

let splice bv lb ub =
  Array.sub bv lb (ub - lb + 1)
