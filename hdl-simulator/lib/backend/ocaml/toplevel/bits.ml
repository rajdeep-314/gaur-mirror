type bit = bool
type bv  = bit array

let concat = Array.append
let ( ++ ) = concat
let splice bv lb ub = Array.sub bv lb (ub - lb + 1)
let nand b1 b2 = not (b1 && b2)

module Chip_Nand = struct
  type tin = < var_a : bv ; var_b : bv >
  type tout = bv

  let out (ip : tin) : tout = [| nand ip#var_a.(0) ip#var_b.(0) |]
end
