*Tests for ram-init command-line errors.*


*** Missing input file ***

  $ ram-init >/dev/null 2>&1; echo $?
  2



*** Too many positional arguments ***

  $ ram-init rams/basic.ram rams/basic.ram >/dev/null 2>&1; echo $?
  1



*** Invalid endianness ***

  $ ram-init rams/basic.ram -e=middle >/dev/null 2>&1; echo $?
  3

*** Address out of bounds ***

*The RAM specification contains an address outside the allocated RAM.*
*This happens when offset + addr >= ram_size (or if a negative address exist).

  $ ram-init rams/basic.ram -rsz=8 -wsz=1 >/dev/null 2>&1; echo $?
  2


*** Help flag (-h) ***

  $ ram-init -h >/dev/null 2>&1; echo $?
  0



*** Help flag (-help) ***

  $ ram-init -help >/dev/null 2>&1; echo $?
  0



*** Help flag (--help) ***

  $ ram-init --help >/dev/null 2>&1; echo $?
  0

