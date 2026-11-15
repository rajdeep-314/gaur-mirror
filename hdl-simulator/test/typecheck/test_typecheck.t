  $ test_typecheck files/Access.ghdl
  [8] Type Error
  File "files/Access.ghdl", line 7, characters 10-13:
  
   7 |   foo := bar
     |          ^^^
  
  Unbound variable bar
  [3]

  $ test_typecheck files/Assign.ghdl
  [1] Type Error
  File "files/Assign.ghdl", line 8, characters 3-21:
  
   8 |   out := {a = temp1}
     |   ^^^^^^^^^^^^^^^^^^
  
  The variable of type bits 1 can not be assigned an expression of type 
  {a: bits 1}
  [3]

  $ test_typecheck files/Call_arg.ghdl
  [0] Type Error
  File "files/Call_arg.ghdl", line 7, characters 10-17:
  
   7 |   foo := Nand in
     |          ^^^^^^^
  
  The expression has type bits 1 but an expression was expected of type 
  {a: bits 1, b: bits 1}
  [3]

  $ test_typecheck files/Call_typ.ghdl

  $ test_typecheck files/Call_unbound.ghdl
  [7] Type Error
  File "files/Call_unbound.ghdl", line 7, characters 10-16:
  
   7 |   foo := Not in
     |          ^^^^^^
  
  Unbound chip Not
  [3]

  $ test_typecheck files/Concat_type.ghdl
  [2] Type Error
  File "files/Concat_type.ghdl", line 14, characters 16-26:
  
   14 |     foobar1 := foo ++ bar
      |                ^^^^^^^^^^
  
  Can not concatenate expressions of types {a: {a: bits 1, b: bits 3}} and 
  {b: {a: bits 1, b: bits 3}}
  [3]

  $ test_typecheck files/Member_field.ghdl
  [9] Type Error
  File "files/Member_field.ghdl", line 7, characters 10-14:
  
   7 |   out := in.c
     |          ^^^^
  
  The record has no field 'c'
  [3]

  $ test_typecheck files/Member_type.ghdl
  [4] Type Error
  File "files/Member_type.ghdl", line 7, characters 10-14:
  
   7 |   foo := in.c
     |          ^^^^
  
  The expression has type bits 1 which is not a record
  [3]

  $ test_typecheck files/Splice_bounds.ghdl
  [6] Type Error
  File "files/Splice_bounds.ghdl", line 14, characters 11-18:
  
   14 |   foo2 := in[4:6]
      |           ^^^^^^^
  
  Splice indices [4, 6] out of range [0, 3]
  [3]

  $ test_typecheck files/Splice_type.ghdl
  [5] Type Error
  File "files/Splice_type.ghdl", line 8, characters 10-18:
  
   8 |   bar := foo[0:1]
     |          ^^^^^^^^
  
  Can not splice an expression of type {a: bits 1, b: bits 1}
  [3]
