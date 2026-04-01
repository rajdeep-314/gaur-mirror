type bitstream = Bits.t Stream.t

let to_bool = Stream.map Bits.to_bool

let rec b_nand (a, b) () =
  let open Stream in
  match (a (), b ()) with
  | Cons (x1, next1),
    Cons (x2, next2) ->
    Cons (
      Bits.nand (x1, x2),
      b_nand (next1, next2)
    )

let b_not a =
  b_nand (a, a)

let b_and (a, b) =
  b_not (b_nand (b_not a, b_not b))

let b_or (a, b) =
  b_not (b_and (b_not a, b_not b))

let b_xor (a, b) =
  b_or ((b_and (a, b_not b)), (b_and (b_not a, b)))

let b_mux (a, b, sel) =
  let sel_bool = to_bool sel in
  Stream.merge sel_bool a b

let b_dmux (i, sel) =
  let a = b_and (b_not sel, i) in
  let b = b_and (sel, i) in
  a, b

let b_dff d =
  let os = Stream.repeat Bits.O in
  Stream.follow os d

let b_dff_general init d =
  let os = Stream.repeat init in
  Stream.follow os d

let b_dff' = b_dff_general Bits.O

let b_bit (d, load) =
  let rec out () =
    b_mux (b_dff out, d, load) ()
  in
  out

let b_general init cmb ip =
  let rec out () =
    cmb (b_dff_general init out, ip) ()
  in
  out

let b_bit'  = b_general Bits.O (fun (dff, (d, load)) -> b_mux (dff, d, load))

let b_reg4 ((d0, d1, d2, d3), load) =
  b_bit (d0, load),
  b_bit (d1, load),
  b_bit (d2, load),
  b_bit (d3, load)

let b_hadd (a, b) =
  b_xor (a, b),
  b_and (a, b)

let b_fadd (a, b, c) =
  let sum1, car1 = b_hadd (a, b) in
  let sum, car2 = b_hadd (sum1, c) in
  let car = b_or (car1, car2) in
  sum, car
