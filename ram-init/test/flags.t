*Tests for the ram-init executable's command line flags.*


*** Short output flag (-o) ***

  $ ram-init rams/basic.ram -rsz=16 -wsz=1 -o basic-short

  $ xxd basic-short
  00000000: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f  ................


*** Long output flag (--out) ***

  $ ram-init rams/basic.ram -rsz=16 -wsz=1 --out basic-long

  $ xxd basic-long
  00000000: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f  ................


*** -o and --out produce identical output ***

  $ cmp basic-short basic-long && echo "no difference"
  no difference


*** Short RAM-size flag (-rsz) ***

  $ ram-init rams/basic.ram -rsz=16 -wsz=1 -o rsz

  $ xxd rsz
  00000000: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f  ................


*** Short word-size flag (-wsz) ***

  $ ram-init rams/basic.ram -rsz=16 -wsz=2 -o wsz

  $ xxd wsz
  00000000: 0000 0001 0002 0003 0004 0005 0006 0007  ................
  00000010: 0008 0009 000a 000b 000c 000d 000e 000f  ................


*** Short endianness flag (-e) ***

  $ ram-init rams/endian.ram -rsz=3 -wsz=4 --endianness=little -o e-long

  $ ram-init rams/endian.ram -rsz=3 -wsz=4 -e=little -o e-short

  $ cmp e-short e-long && echo "no difference"
  no difference


*** Short offset flag (-off) ***

  $ ram-init rams/sparse.ram -rsz=10 -wsz=1 -off=2 -o off-short

  $ ram-init rams/sparse.ram -rsz=10 -wsz=1 --offset=2 -o off-long

  $ cmp off-short off-long && echo "no difference"
  no difference


*** Short padding flag (-p) ***

  $ ram-init rams/sparse.ram -rsz=8 -wsz=1 -p=255 -o p-short

  $ ram-init rams/sparse.ram -rsz=8 -wsz=1 --padding=255 -o p-long

  $ cmp p-short p-long && echo "no difference"
  no difference
