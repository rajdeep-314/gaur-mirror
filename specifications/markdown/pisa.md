---
title: "PISA"
colorlinks: true
linkcolor: blue
urlcolor: blue
toccolor: blue
citecolor: blue
geometry: margin=1cm
mainfont: "Ubuntu Mono"
geometry: margin=1cm
header-includes:
  - \renewcommand{\familydefault}{\ttdefault}
---


## Instruction Set

|   S. No.  |   Instruction name    |       Syntax      |   Meaning                     |           Format              |
|-----------|-----------------------|-------------------|-------------------------------|-------------------------------|
|   1.      |   lli                 |   lli rd imm      |   rd[7:0] = imm               |   0000_rd(4)_imm(8)           |
|   2.      |   lui                 |   lui rd imm      |   rd[15:8] = imm              |   0001_rd(4)_imm(8)           |
|   3.      |   ld                  |   ld rd rs imm    |   rd = RAM[rs + imm]          |   0010_rd(4)_rs(4)_imm(4)     |
|   4.      |   st                  |   st rs1 rs2 imm  |   RAM[rs2 + imm] = rs1        |   0011_rs1(4)_rs2(4)_imm(4)   |
|   5.      |   add                 |   add rd rs1 rs2  |   rd = rs1 + rs2              |   0100_rd(4)_rs1(4)_rs2(4)    |
|   6.      |   addi                |   addi rd rs imm  |   rd = rs + imm               |   0101_rd(4)_rs(4)_imm(4)     |
|   7.      |   sub                 |   sub rd rs1 rs2  |   rd = rs1 - rs2              |   0110_rd(4)_rs1(4)_rs2(4)    |
|   8.      |   and                 |   and rd rs1 rs2  |   rd = rs1 & rs2              |   0111_rd(4)_rs1(4)_rs2(4)    |
|   9.      |   not                 |   not rd rs       |   rd = ~ rs1                  |   1000_rd(4)_rs1(4)_0000      |
|   10.     |   beq                 |   beq rs1 rs2 imm |   if rs1 == rs2, PC += imm    |   1001_rs1(4)_rs2(4)_imm(4)   |
|   11.     |   blt                 |   blt rs1 rs2 imm |   if rs1 < rs2, PC += imm     |   1010_rs1(4)_rs2(4)_imm(4)   |
|   12.     |   jalr                |   jalr rd rs imm  |   rd = PC + 1, PC = rs + imm  |   1011_rd(4)_rs(4)_imm(4)     |
|   13.     |   apc                 |   apc rd imm      |   rd += PC + imm              |   1100_rd(4)_imm(8)           |
|   14.     |   j                   |   j imm           |   PC += imm                   |   1101_imm(12)                |


## Registers


|   Register    |   Alias       |   Convention      |
|---------------|---------------|-------------------|
|   r0          |   zero        |   -               |
|   r1          |   ra          |   Return address  |
|   r2          |   sp          |   Stack pointer   |
|   r3 - r4     |   -           |   -               |
|   r5 - r12    |   t0 - t7     |   Temporaries     |
|   r13 - r15   |   -           |   -               |


## Notes

- We have sixteen general purpose `16-bit` integer registers. They can be referred to using the names `r0`, `r1`,..., `r15`.
- `r0` is the zero register. Reading from it always gives `0`.
- For `lli` and `lui`, `imm` is an unsigned immediate value, and it must belong to the range `[0, 255]`.
- For the branch instructions (`beq` and `blt`), the immediate is a signed immediate with the range `[-8, 7]`.
- For `ld` and `st`, `imm` is meant to provide the signed address offset, and it must be in the interval `[0, 15]`.
- For `addi`, `imm` is the signed immediate value, and it must belong to the interval `[-8, 7]`.
- For `jalr`, `imm` is the signed offset value, and it must belong to the range `[-8, 7]`.
- For `apc`, `imm` is the signed offset value belonging to the range `[-128, 127]`.
- `j` is the jump instruction and it's immediate is a signed immediate belonging to the range `[-2048, 2047]`.
- For the VM implementation, `r3` points to the `local` segment, and `r4` to the `argument` segment.

