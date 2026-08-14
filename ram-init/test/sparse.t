*Tests sparse RAM mappings.*


*** Unspecified addresses default to zero ***

  $ ram-init rams/sparse.ram -rsz=8 -wsz=1 -e=big -o sparse

  $ xxd sparse
  00000000: 1100 0022 0000 0044                      ..."...D

