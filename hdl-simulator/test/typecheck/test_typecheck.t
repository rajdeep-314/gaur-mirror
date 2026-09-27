  $ test_typecheck files/Access.phdl
  [8] Type Error
  File "files/Access.phdl", line 7, characters 10-13:
  
  Unbound variable bar
  [3]

  $ test_typecheck files/Assign.phdl
  [1] Type Error
  File "files/Assign.phdl", line 8, characters 3-21:
  
  The variable of type bits 1 can not be assigned an expression of type 
  {a: bits 1}
  [3]

  $ test_typecheck files/Call_arg.phdl
  [0] Type Error
  File "files/Call_arg.phdl", line 7, characters 10-17:
  
  The expression has type bits 1 but an expression was expected of type 
  {a: bits 1, b: bits 1}
  [3]

  $ test_typecheck files/Call_typ.phdl

  $ test_typecheck files/Call_unbound.phdl
  [7] Type Error
  File "files/Call_unbound.phdl", line 7, characters 10-16:
  
  Unbound chip Not
  [3]

  $ test_typecheck files/Concat_type.phdl
  [2] Type Error
  File "files/Concat_type.phdl", line 14, characters 16-26:
  
  Can not concatenate expressions of types {a: {a: bits 1, b: bits 3}} and 
  {b: {a: bits 1, b: bits 3}}
  [3]

  $ test_typecheck files/Member_field.phdl
  [9] Type Error
  File "files/Member_field.phdl", line 7, characters 10-14:
  
  The record has no field 'c'
  [3]

  $ test_typecheck files/Member_type.phdl
  [4] Type Error
  File "files/Member_type.phdl", line 7, characters 10-14:
  
  The expression has type bits 1 which is not a record
  [3]

  $ test_typecheck files/Splice_bounds.phdl
  [6] Type Error
  File "files/Splice_bounds.phdl", line 14, characters 11-18:
  
  Splice indices [4, 6] out of range [0, 3]
  [3]

  $ test_typecheck files/Splice_type.phdl
  [5] Type Error
  File "files/Splice_type.phdl", line 8, characters 10-18:
  
  Can not splice an expression of type {a: bits 1, b: bits 1}
  [3]
