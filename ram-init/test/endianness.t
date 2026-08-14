*Tests for RAM word encoding and endianness.*


*** Big-endian encoding ***

  $ ram-init rams/endian.ram -rsz=3 -wsz=4 -e=big -o endian-big

  $ xxd endian-big
  00000000: 0000 1234 0000 abcd 1234 5678            ...4.....4Vx



*** Little-endian encoding ***

  $ ram-init rams/endian.ram -rsz=3 -wsz=4 -e=little -o endian-little

  $ xxd endian-little
  00000000: 3412 0000 cdab 0000 7856 3412            4.......xV4.

