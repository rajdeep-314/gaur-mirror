  $ test_causality files/valid_comb.ghdl

  $ test_causality files/valid_seq.ghdl

  $ test_causality files/self_loop.ghdl
  [0] Cyclic Dependency Error
  File "files/self_loop.ghdl", line 8, characters 1-76:
  
   8  | CHIP SelfLoop {
   9  |   INPUT in: bits 1
   10 |   OUTPUT out: bits 1
   11 | 
   12 |   out := Not out
   13 | }
  
  Instantaneous cycle detected in chip 'SelfLoop':
    out -> out
  Cycle path breakdown:
    * out -> out via call to 'Not' at File "files/self_loop.ghdl", line 12, characters 3-17: (in -> out)
  [4]

  $ test_causality files/cross_nand.ghdl
  [0] Cyclic Dependency Error
  File "files/cross_nand.ghdl", line 1, characters 1-210:
  
   1 | CHIP CrossNand {
   2 |   INPUT in: { s: bits 1, r: bits 1 }
   3 |   OUTPUT out: { q: bits 1, not_q: bits 1 }
   4 | 
   5 |   q := Nand { a = in.s, b = out.not_q }
   6 |   not_q := Nand { a = in.r, b = q }
   7 |   out := { q = q, not_q = not_q }
   8 | }
  
  Instantaneous cycle detected in chip 'CrossNand':
    q -> not_q -> out.not_q -> q
  Cycle path breakdown:
    * q -> not_q via call to 'Nand' at File "files/cross_nand.ghdl", line 6, characters 3-36: (in.b -> out)
    * not_q -> out.not_q via assignment at File "files/cross_nand.ghdl", line 7, characters 3-34:
    * out.not_q -> q via call to 'Nand' at File "files/cross_nand.ghdl", line 5, characters 3-40: (in.b -> out)
  [4]

  $ test_causality files/cross_subchips.ghdl
  [0] Cyclic Dependency Error
  File "files/cross_subchips.ghdl", line 9, characters 1-95:
  
   9  | CHIP LoopSubchips {
   10 |   INPUT in: bits 1
   11 |   OUTPUT out: bits 1
   12 | 
   13 |   w := Pass out
   14 |   out := Pass w
   15 | }
  
  Instantaneous cycle detected in chip 'LoopSubchips':
    w -> out -> w
  Cycle path breakdown:
    * w -> out via call to 'Pass' at File "files/cross_subchips.ghdl", line 14, characters 3-16: (in -> out)
    * out -> w via call to 'Pass' at File "files/cross_subchips.ghdl", line 13, characters 3-16: (in -> out)
  [4]

  $ test_causality files/independent_record_fields.ghdl

  $ test_causality files/ring_feedback_with_seq.ghdl

  $ test_causality files/diamond_dag.ghdl

  $ test_causality files/multi_hop_cycle.ghdl
  [0] Cyclic Dependency Error
  File "files/multi_hop_cycle.ghdl", line 22, characters 1-120:
  
   22 | CHIP Ring3 {
   23 |   INPUT in: bits 1
   24 |   OUTPUT out: bits 1
   25 | 
   26 |   w1 := StepA out
   27 |   w2 := StepB w1
   28 |   w3 := StepC w2
   29 |   out := w3
   30 | }
  
  Instantaneous cycle detected in chip 'Ring3':
    w3 -> out -> w1 -> w2 -> w3
  Cycle path breakdown:
    * w3 -> out via assignment at File "files/multi_hop_cycle.ghdl", line 29, characters 3-12:
    * out -> w1 via call to 'StepA' at File "files/multi_hop_cycle.ghdl", line 26, characters 3-18: (in -> out)
    * w1 -> w2 via call to 'StepB' at File "files/multi_hop_cycle.ghdl", line 27, characters 3-17: (in -> out)
    * w2 -> w3 via call to 'StepC' at File "files/multi_hop_cycle.ghdl", line 28, characters 3-17: (in -> out)
  [4]

  $ test_causality files/record_field_cross_cycle.ghdl
  [0] Cyclic Dependency Error
  File "files/record_field_cross_cycle.ghdl", line 1, characters 1-188:
  
   1 | CHIP CrossFields {
   2 |   INPUT in: { a: bits 1, b: bits 1 }
   3 |   OUTPUT out: { a: bits 1, b: bits 1 }
   4 | 
   5 |   x := { a = out.b, b = in.b }
   6 |   y := { a = in.a, b = x.a }
   7 |   out := { a = x.b, b = y.b }
   8 | }
  
  Instantaneous cycle detected in chip 'CrossFields':
    y.b -> out.b -> x.a -> y.b
  Cycle path breakdown:
    * y.b -> out.b via assignment at File "files/record_field_cross_cycle.ghdl", line 7, characters 3-30:
    * out.b -> x.a via assignment at File "files/record_field_cross_cycle.ghdl", line 5, characters 3-31:
    * x.a -> y.b via assignment at File "files/record_field_cross_cycle.ghdl", line 6, characters 3-29:
  [4]

  $ test_causality files/bus_slice_cycle.ghdl
  [0] Cyclic Dependency Error
  File "files/bus_slice_cycle.ghdl", line 1, characters 1-125:
  
   1 | CHIP BusLoop {
   2 |   INPUT in: bits 2
   3 |   OUTPUT out: bits 2
   4 | 
   5 |   w1 := out[0:0] ++ in[0:0]
   6 |   w2 := in[1:1] ++ w1[1:1]
   7 |   out := w2
   8 | }
  
  Instantaneous cycle detected in chip 'BusLoop':
    w2 -> out -> w1 -> w2
  Cycle path breakdown:
    * w2 -> out via assignment at File "files/bus_slice_cycle.ghdl", line 7, characters 3-12:
    * out -> w1 via assignment at File "files/bus_slice_cycle.ghdl", line 5, characters 3-28:
    * w1 -> w2 via assignment at File "files/bus_slice_cycle.ghdl", line 6, characters 3-27:
  [4]

