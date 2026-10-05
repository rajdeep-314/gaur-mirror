# Gaur ASM

## Description

`Gaur ASM` is the assembly language for `Gaur ISA` -- it can be assembled to generate
`Gaur ISA` binaries.

Every `Gaur ISA` instruction other than `beq`, `blt`, and `j` directly correspond
to a `Gaur ASM` instruction. These instructions are written in `Gaur ASM` exactly like
their textual representations (see Gaur ISA specification for the syntax). `beq`,
`blt`, and `j` are written in `Gaur ASM` with labels instead of immediates. See
[Notes](#notes) for more details on labels.

In addition to these 15 instructions, a pseudo-instruction `spin` has been
provided.


## Pseudo-instructions

|   S. No.  |   Pseudo-instruction syntax       |       Expansion           |
|-----------|-----------------------------------|---------------------------|
|   1.      |       spin                        |       j 0                 |


## Notes

- Each line in a valid `Gaur ASM` program is either a label declaration or an instruction/pseudo-instruction (or an empty line).
- Anything that starts with a `#` is treated as a single-line comment.
- To attach a location in the program to a label `label_x`, add a line `label_x:` for that location.
- Label names must begin with a Latin alphabet, or an underscore, or a period, and can be followed by a string consisting of alphanumeric characters, underscore, hyphen, period and dollar sign. The corresponding regular expression is `[a-zA-Z_.][a-zA-Z0-9\-_.\$]*`.
- Out-of-bound labels for labelled instructions lead to invalid `Gaur ASM` programs. The assembler does not assemble such programs.
- Branch instructions have a small immediate range. However, they can be paired with `j` instructions to effectively expand the range.


$\pagebreak$


## Examples

### Fibonacci

The following `Gaur ASM` program computes the first `n` Fibonacci numbers and place them in the main memory, starting at address `k`, where initially, `RAM[0] = n` and `RAM[1] = k`.

```text
ld r1 r0 0
ld r2 r0 1

addi r3 r0 0
addi r5 r0 0
addi r6 r0 1

loop:
    beq r3 r1 halt
    add r4 r2 r3
    st r5 r4 0
    add r6 r6 r5
    sub r5 r6 r5
    addi r3 r3 1
    j loop

halt:
    j halt
```

The final two lines

```text
halt:
    j halt
```

can be replaced by the single line

```text
spin
```


### Even or odd

The following program puts `1` in `r4` if `r3` is even, and `-1` otherwise.

```text
addi r1 r0 1
addi r3 r0 3

and r1 r3 r1
beq r1 r0 is_even
addi r4 r0 -1
j halt

is_even:
addi r4 r0 1

halt:
    j halt
```


$\pagebreak$


## Expanding the immediate range

Consider the following program where the jump target is outside the `beq` immediate range.

```text
beq r0 r0 halt

# 42 very useful instructions.
addi r0 r0 0
addi r0 r0 0
.
.
.
addi r0 r0 0
addi r0 r0 0

halt:
    j halt
```

Assembling this directly would fail, as the target instruction is 43 instructions ahead of the branch instruction. To overcome this, couple the `beq` instruction with a `j` instruction as follows:

```text
beq r0 r0 temp
j halt
temp:

# 42 very useful instructions.
addi r0 r0 0
addi r0 r0 0
.
.
.
addi r0 r0 0
addi r0 r0 0

halt:
    j halt
```

