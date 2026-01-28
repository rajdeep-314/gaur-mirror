# PASM


## Description

`PASM` is the assembly language for `PISA` -- it can be assembled to generate `PISA` binaries.

Every `PISA` instruction other than `beq`, `blt`, and `j` directly correspond to a `PASM` instruction. These instructions are written in `PASM` exactly like their textual representations (see PISA specification for the syntax). `beq`, `blt`, and `j` are written in `PASM` with labels instead of immediates. See [Notes](#notes) for more details on labels.

In addition to these 14 instructions, a pseudo-instruction `spin` has been provided.


## Pseudo-instructions

|   S. No.  |   Pseudo-instruction syntax       |       Expansion           |
|-----------|-----------------------------------|---------------------------|
|   1.      |       spin                        |       j 0                 |


## Notes

- Each line in a valid `PASM` program is either a label declaration or an instruction/pseudo-instruction (or an empty line).
- Anything that starts with a `#` is treated as a single-line comment.
- To attach a location in the program to a label `label_x`, add a line `label_x:` for that location.
- Label names must begin with a Latin alphabet, or an underscore, or a period, and can be followed by a string consisting of alphanumeric characters, underscore, hyphen, period and dollar sign. The corresponding regular expression is `[a-zA-Z_.][a-zA-Z0-9\-_.\$]*`.
- Out-of-bound labels for labelled instructions lead to invalid `PASM` programs. The assembler does not assemble such programs.
- Branch instructions have a small immediate range. However, they can be paired with `j` instructions to effectively expand the range.


$\pagebreak$


## Examples

### Fibonacci

The following `PASM` program computes the first `n` Fibonacci numbers and place them in the main memory, starting at address `k`, where initially, `RAM[0] = n` and `RAM[1] = k`.

```pasm
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

```pasm
halt:
    j halt
```

can be replaced by the single line

```pasm
spin
```


### Even or odd

The following program puts `1` in `r4` if `r3` is even, and `-1` otherwise.

```pasm
lli r1 1
lui r1 0

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

```pasm
beq r0 r0 halt

# 10 very useful instructions.
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0

halt:
    j halt
```

Assembling this directly would fail, as the target instruction is 11 instructions ahead of the branch instruction. To overcome this, couple the `beq` instruction with a `j` instruction as follows:

```pasm
beq r0 r0 temp
j halt
temp:

# 10 very useful instructions.
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0
addi r0 r0 0

halt:
    j halt
```

