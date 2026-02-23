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
|   1.      |   lli                 |   lli rd imm      |   r31[15:0] = imm, r31[31:16] = 0 |   h<01>_imm(16)                   |
|   2.      |   lui                 |   lui rd imm      |   r31[15:8] = imm                 |   h<02>_imm(16)                   |
|   3.      |   ld                  |   ld rd rs1 imm   |   rd = RAM[rs1 + imm]             |   h<03>_rd(5)_rs1(5)_imm(6)       |
|   4.      |   st                  |   st rs1 rs2 imm  |   RAM[rs2 + imm] = rs1            |   h<04>_rs1(5)_rs2(5)_imm(6)      |
|   5.      |   add                 |   add rd rs1 rs2  |   rd = rs1 + rs2                  |   h<05>_rd(5)_rs1(5)_rs2(5)_0     |
|   6.      |   addi                |   addi rd rs1 imm |   rd = rs1 + imm                  |   h<06>_rd(5)_rs1(5)_imm(6)       |
|   7.      |   sub                 |   sub rd rs1 rs2  |   rd = rs1 - rs2                  |   h<07>_rd(5)_rs1(5)_rs2(5)_0     |
|   8.      |   and                 |   and rd rs1 rs2  |   rd = rs1 & rs2                  |   h<08>_rd(5)_rs1(5)_rs2(5)_0     |
|   9.      |   not                 |   not rd rs1      |   rd = ~ rs1                      |   h<09>_rd(5)_rs1(5)_000000       |
|   10.     |   beq                 |   beq rs1 rs2 imm |   if rs1 == rs2, PC += imm        |   h<10>_rs1(5)_rs2(5)_imm(6)      |
|   11.     |   blt                 |   blt rs1 rs2 imm |   if rs1 < rs2, PC += imm         |   h<11>_rs1(5)_rs2(5)_imm(6)      |
|   12.     |   jalr                |   jalr rd rs1 imm |   rd = PC + 1, PC = rs1 + imm     |   h<12>_rd(5)_rs1(5)_imm(6)       |
|   13.     |   apc                 |   apc rd imm      |   rd += PC + imm                  |   h<13>_rd(5)_imm(11)             |
|   14.     |   j                   |   j imm           |   PC += imm                       |   h<14>_imm(16)                   |


## Notes

- There are 32 general-purpose integer registers. Each register is 32-bit wide.
- The main memory (RAM) is word-addressed, one word being 32-bits wide.
- `h<n>` is `n` in one-hot encoding -- $16$ bits, of which the $n^{\mathrm{th}}$ LSB (from the right) is $1$, rest all are $0$.
- `r0` is the zero register. Reading from it always gives 0. Writing to it has no effect.
- For every non-control instruction, an implicit side effect is `PC += 1`.
- For `lli` and `lui`, `imm` is an unsigned immediate value, and it must belong to the range [0, $2^{16} - 1$].
- For `lli` and `lui`, the loading is done in the register `r31`.
- For the branch instructions (`beq` and `blt`), the immediate is a signed immediate with the range [-1024, 1023].
- For `ld` and `st`, `imm` is meant to provide the signed address offset, and it must be in the interval [-32, 31].
- For `addi`, `imm` is the signed immediate value, and it must belong to the interval [-32, 31].
- For `jalr`, `imm` is the signed offset value, and it must belong to the range [-32, 31].
- For `apc`, `imm` is the signed offset value belonging to the range [-1024, 1023].
- `j` is the jump instruction and its immediate is a signed immediate belonging to the range [$-2^{15}$, $2^{15} - 1$].

