# PISA


## Description

A PISA processor has 16 general-purpose integer registers, named r0 through r15. It also includes an integer PC register -- the program counter, which is implicit in all instructions. The design is inspired by [RISC-V](https://riscv.org/).

Each register is 16 bits wide. The main memory is word-addressable, with each word being 2 bytes. Thus, the processor can address $2^{16}$ words, corresponding to 128 KB of main memory.


## Instruction set

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
|   9.      |   not                 |   not rd rs       |   rd = ~ rs                   |   1000_rd(4)_rs(4)_0000       |
|   10.     |   beq                 |   beq rs1 rs2 imm |   if rs1 == rs2, PC += imm    |   1001_rs1(4)_rs2(4)_imm(4)   |
|   11.     |   blt                 |   blt rs1 rs2 imm |   if rs1 < rs2, PC += imm     |   1010_rs1(4)_rs2(4)_imm(4)   |
|   12.     |   jalr                |   jalr rd rs imm  |   rd = PC + 1, PC = rs + imm  |   1011_rd(4)_rs(4)_imm(4)     |
|   13.     |   apc                 |   apc rd imm      |   rd += PC + imm              |   1100_rd(4)_imm(8)           |
|   14.     |   j                   |   j imm           |   PC += imm                   |   1101_imm(12)                |


## Notes

- `r0` is the zero register. Reading from it always gives 0. Writing to it has no effect.
- For every non-control instruction, an implicit side effect is `PC += 1`.
- For `lli` and `lui`, `imm` is an unsigned immediate value, and it must belong to the range [0, 255].
- For the branch instructions (`beq` and `blt`), the immediate is a signed immediate with the range [-8, 7].
- For `ld` and `st`, `imm` is meant to provide the signed address offset, and it must be in the interval [-8, 7].
- For `addi`, `imm` is the signed immediate value, and it must belong to the interval [-8, 7].
- For `jalr`, `imm` is the signed offset value, and it must belong to the range [-8, 7].
- For `apc`, `imm` is the signed offset value belonging to the range [-128, 127].
- `j` is the jump instruction and its immediate is a signed immediate belonging to the range [-2048, 2047].


## Binary files

The binary encoding of PISA programs is simple and direct. A PISA program is simply a sequence of PISA instructions. Mapping each PISA instruction to two bytes in big-endian and concatenating them together transforms a PISA program into its binary representation.


## Example

Following is textual representation of a PISA program to compute the first `n` Fibonacci numbers and place them in the main memory, starting at address `k`, where initially, `RAM[0] = n` and `RAM[1] = k`.

```
ld r1 r0 0
ld r2 r0 1
addi r3 r0 0
addi r5 r0 0
addi r6 r0 1
beq r3 r1 7
add r4 r2 r3
st r5 r4 0
add r6 r6 r5
sub r5 r6 r5
addi r3 r3 1
j -6
j 0
```

Mapping each instruction to the ASCII representation of it's binary layout gives the following program:

```
0010000100000000
0010001000000001
0101001100000000
0101010100000000
0101011000000001
1001001100010111
0100010000100011
0011010101000000
0100011001100101
0110010101100101
0101001100110001
1101111111111010
1101000000000000
```

