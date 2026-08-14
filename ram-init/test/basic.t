*Basic tests for RAM image generation.*


*** Basic address-to-value mapping ***

  $ ram-init rams/basic.ram -rsz=16 -wsz=1 -e=big -o basic

  $ xxd basic
  00000000: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f  ................



*** Basic mapping with four-byte words ***

  $ ram-init rams/basic.ram -rsz=16 -wsz=4 -e=big -o basic4

  $ xxd -l 64 basic4
  00000000: 0000 0000 0000 0001 0000 0002 0000 0003  ................
  00000010: 0000 0004 0000 0005 0000 0006 0000 0007  ................
  00000020: 0000 0008 0000 0009 0000 000a 0000 000b  ................
  00000030: 0000 000c 0000 000d 0000 000e 0000 000f  ................

