*Tests padding of unspecified RAM addresses.*


*** Padding with zero ***

  $ ram-init rams/sparse.ram -rsz=8 -wsz=1 -p=0 -o padding-zero

  $ xxd padding-zero
  00000000: 1100 0022 0000 0044                      ..."...D



*** Padding with 255 ***

  $ ram-init rams/sparse.ram -rsz=8 -wsz=1 -p=255 -o padding-ff

  $ xxd padding-ff
  00000000: 11ff ff22 ffff ff44                      ..."...D

