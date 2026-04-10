  $ test_typecheck files/Access.phdl
  File "files/Access.phdl", line 7, characters 10-13:
  Unbound variable bar
  [2]

  $ test_typecheck files/Assign.phdl
  File "files/Assign.phdl", line 8, characters 3-21:
  The variable of type bits 1 can not be assigned an expression of type 
  {a: bits 1}
  [2]

  $ test_typecheck files/Call_arg.phdl
  File "files/Call_arg.phdl", line 7, characters 10-17:
  The expression has type bits 1 but an expression was expected of type 
  {a: bits 1, b: bits 1}
  [2]

  $ test_typecheck files/Call_typ.phdl

  $ test_typecheck files/Call_unbound.phdl
  File "files/Call_unbound.phdl", line 7, characters 10-16:
  Unbound chip Not
  [2]

  $ test_typecheck files/Concat_type.phdl
  File "files/Concat_type.phdl", line 14, characters 16-26:
  Can not concatenate expressions of types {a: {a: bits 1, b: bits 3}} and 
  {b: {a: bits 1, b: bits 3}}
  [2]

  $ test_typecheck files/Member_field.phdl
  File "files/Member_field.phdl", line 7, characters 10-14:
  The record has no field 'c'
  [2]

  $ test_typecheck files/Member_type.phdl
  File "files/Member_type.phdl", line 7, characters 10-14:
  The expression has type bits 1 which is not a record
  [2]

  $ test_typecheck files/Splice_bounds.phdl
  File "files/Splice_bounds.phdl", line 14, characters 11-18:
  Splice indices [4, 6] out of range [0, 3]
  [2]

  $ test_typecheck files/Splice_type.phdl
  File "files/Splice_type.phdl", line 8, characters 10-18:
  Can not splice an expression of type {a: bits 1, b: bits 1}
  [2]
