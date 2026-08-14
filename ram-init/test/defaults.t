*Tests the default command-line values.*


*** Default RAM configuration ***

*The defaults are 16384 RAM words, 4-byte words, big-endian,
zero offset, and zero padding.*

  $ ram-init rams/default.ram | xxd -l 16
  00000000: 1234 5678 0000 0000 0000 0000 0000 0000  .4Vx............

