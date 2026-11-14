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
