  $ test_parse files/basic.ghdl

  $ test_parse files/comments.ghdl

  $ test_parse files/intermediate.ghdl

  $ test_parse files/states.ghdl

  $ test_parse files/err_missing_chip_name.ghdl
  [-1] Parsing Error
  File "files/err_missing_chip_name.ghdl", line 1, characters 6-7:
  
   1 | CHIP {
     |      ^
  
  I am expecting a chip name (uppercase identifier) after 'CHIP'.
  [2]



  $ test_parse files/err_missing_lbrace.ghdl
  [-1] Parsing Error
  File "files/err_missing_lbrace.ghdl", line 2, characters 3-8:
  
   2 |   INPUT in: bits 1
     |   ^^^^^
  
  I am expecting '{' after the chip name to open the chip body.
  [2]



  $ test_parse files/err_missing_input.ghdl
  [-1] Parsing Error
  File "files/err_missing_input.ghdl", line 2, characters 3-9:
  
   2 |   OUTPUT out: bits 1
     |   ^^^^^^
  
  I am expecting a chip body beginning with 'INPUT'
  after reading '{'.
  [2]



  $ test_parse files/err_missing_output.ghdl
  [-1] Parsing Error
  File "files/err_missing_output.ghdl", line 3, characters 1-2:
  
   3 | }
     | ^
  
  I am expecting an output declaration beginning with 'OUTPUT'
  after the input declaration.
  [2]



  $ test_parse files/err_bad_assign.ghdl
  [-1] Parsing Error
  File "files/err_bad_assign.ghdl", line 6, characters 1-2:
  
   6 | }
     | ^
  
  I am expecting a right-hand side expression after ':='.
  [2]



  $ test_parse files/err_duplicate_key.ghdl
  File "files/err_duplicate_key.ghdl", line 2, characters 26-27:
  Duplicate key a
  [1]

  $ test_parse files/err_lex_bad_char.ghdl
  Unexpected char '@'.
  [1]
