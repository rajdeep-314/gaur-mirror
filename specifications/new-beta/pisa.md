---
title: "New PISA Proposal"
colorlinks: true
linkcolor: blue
urlcolor: blue
toccolor: blue
citecolor: blue
geometry: top=1cm, left=1cm, right=1cm, bottom=2cm
mainfont: "Ubuntu Mono"
header-includes:
  - \renewcommand{\familydefault}{\ttdefault}
  - \usepackage{xcolor}
  - \definecolor{shadecolor}{RGB}{237, 233, 204}
  - \usepackage{framed}
---


## Instruction set

|   S. No.  |   Instruction name    |       Syntax      |   Meaning                         |           Format                  |
|-----------|-----------------------|-------------------|-----------------------------------|-----------------------------------|
|   1.      |   li                  |   li rd imm       |   rd = imm                        |   h<01>_rd(5)_imm(11)             |
|   2.      |   ld                  |   ld rd rs1 imm   |   rd = RAM[rs1 + imm]             |   h<02>_rd(5)_rs1(5)_imm(6)       |
|   3.      |   st                  |   st rs1 rs2 imm  |   RAM[rs2 + imm] = rs1            |   h<03>_rs1(5)_rs2(5)_imm(6)      |
|   4.      |   add                 |   add rd rs1 rs2  |   rd = rs1 + rs2                  |   h<04>_rd(5)_rs1(5)_rs2(5)_0     |
|   5.      |   addi                |   addi rd rs1 imm |   rd = rs1 + imm                  |   h<05>_rd(5)_rs1(5)_imm(6)       |
|   6.      |   sub                 |   sub rd rs1 rs2  |   rd = rs1 - rs2                  |   h<06>_rd(5)_rs1(5)_rs2(5)_0     |
|   7.      |   and                 |   and rd rs1 rs2  |   rd = rs1 & rs2                  |   h<07>_rd(5)_rs1(5)_rs2(5)_0     |
|   8.      |   not                 |   not rd rs1      |   rd = ~ rs1                      |   h<08>_rd(5)_rs1(5)_000000       |
|   9.      |   sl                  |   sl rd rs imm    |   rd = rs << imm                  |   h<09>_rd(5)_rs(5)_imm(6)        |
|   10.     |   sr                  |   sr rd rs imm    |   rd = rs >> imm                  |   h<10>_rd(5)_rs(5)_imm(6)        |
|   11.     |   beq                 |   beq rs1 rs2 imm |   if rs1 == rs2, PC += imm        |   h<11>_rs1(5)_rs2(5)_imm(6)      |
|   12.     |   blt                 |   blt rs1 rs2 imm |   if rs1 < rs2, PC += imm         |   h<12>_rs1(5)_rs2(5)_imm(6)      |
|   13.     |   jalr                |   jalr rd rs1 imm |   rd = PC + 1, PC = rs1 + imm     |   h<13>_rd(5)_rs1(5)_imm(6)       |
|   14.     |   apc                 |   apc rd imm      |   rd += PC + imm                  |   h<14>_rd(5)_imm(11)             |
|   15.     |   j                   |   j imm           |   PC += imm                       |   h<15>_imm(16)                   |


## Notes

- There are 32 general-purpose integer registers. Each register is 32-bit wide.
- The main memory (RAM) is word-addressed, one word being 32-bits wide.
- `h<n>` is `n` in one-hot encoding -- $16$ bits, of which the $n^{\mathrm{th}}$ LSB (from the right) is $1$, rest all are $0$.
- `r0` is the zero register. Reading from it always gives 0. Writing to it has no effect.
- For every non-control instruction, an implicit side effect is `PC += 1`.
- Left and right shift instructions `sl` and `sr` are both logical shifts.
- For `li`, `imm` is an unsigned immediate value, and it must belong to the range [0, $2^{11} - 1$].
- For the branch instructions (`beq` and `blt`), the immediate is a signed immediate with the range [-1024, 1023].
- For `ld` and `st`, `imm` is meant to provide the signed address offset, and it must be in the interval [-32, 31].
- For `addi`, `imm` is the signed immediate value, and it must belong to the interval [-32, 31].
- For `jalr`, `imm` is the signed offset value, and it must belong to the range [-32, 31].
- For `apc`, `imm` is the signed offset value belonging to the range [-1024, 1023].
- `j` is the jump instruction and its immediate is a signed immediate belonging to the range [$-2^{15}$, $2^{15} - 1$].

