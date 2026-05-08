(**
    Runtime library for bits and bitvectors
*)

(** A singluar bit *)
type bit = bool
let pp_bit ppf b = Fmt.int ppf (Bool.to_int b)

(** A bit vector *)
type bv  = bit array
let pp_bv ppf bv = Fmt.pf ppf "@[<h>%a@]" Fmt.(array pp_bit) bv

(** [concat b₁ b₂] is concatenation of [b₁] and [b₂], [b₁b₂]. *)
let concat = Array.append
let ( ++ ) = concat

(** [splice b l r] is the bitvector [b] from index [l] to [r], both
    inclusive.
*)
let splice bv lb ub = Array.sub bv lb (ub - lb + 1)

(** [nand b₁ b₂] is [not (b₁ && b₂)]. *)
let nand b1 b2 = not (b1 && b2)

module Chip_Nand = struct
  type tin = < var_a : bv ; var_b : bv >
  type tout = bv

  let out (ip : tin) : tout = [| nand ip#var_a.(0) ip#var_b.(0) |]
end
