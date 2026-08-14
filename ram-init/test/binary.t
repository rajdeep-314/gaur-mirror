*Tests that ram-init produces the correct binary encoding.*


*** Basic binary encoding ***

  $ bash rams/binary.sh
  Generated: expected.bin

  $ ram-init rams/basic.ram -rsz=16 -wsz=2 -e=big -o actual.bin

  $ cmp expected.bin actual.bin && echo "binary matches"
  binary matches
