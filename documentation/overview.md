# Overview

## Why Gaur?

The course teaches how computers work, from hardware and circuits up to
assembly, assemblers, virtual machines, and high-level languages. Gaur is
designed around what a teaching toolchain needs:

- **Helpful errors**: messages point at the offending line with a code snippet.
- **Scriptable**: stable flags and documented exit codes, so tools can be used
  for automated assignment evaluation.
- **Easy to adopt**: a small, conventional ISA and a consistent workflow
  across all tools.
- **Correct**: core logic is written in the Rocq theorem prover and extracted
  to OCaml, which opens the door to formal verification.

## The toolchain

| Tool | What it does |
|------|--------------|
| **Assembler** | Assembles Gaur assembly programs to binary, ASCII binary, or an intermediate form with labels resolved. |
| **Disassembler** | Converts binary programs back to readable assembly. Useful for debugging your own assembler. |
| **RAM initializer** | Builds binary RAM images from a simple `address : value` description. |
| **CPU simulator** | Runs programs step by step with traces, final state, and an interactive debugger. |
| **VM emulator** | Runs stack-based VM programs with a debugger, breakpoints, and a character screen. |
| **VM translator** | Translates VM programs to Gaur assembly. *(in progress)* |

## How the pieces fit together

```{mermaid}
flowchart LR
    VM["VM source"] --> TR["VM translator"]
    TR --> ASM["Gaur assembly"]
    ASM --> AS["Assembler"]
    AS --> BIN["Binary"]

    RI["RAM initializer"] --> RAM["RAM image"]

    BIN --> CPU["CPU simulator"]
    RAM --> CPU

    BIN --> DIS["Disassembler"]
    DIS --> INT["Assembly with labels resolved"]
```

## The Gaur ISA at a glance

- RISC-style, with **32 general-purpose registers** (`r0` is hard-wired to zero).
- **32-bit** instructions and words.
- **One-hot encoded opcodes**, which removes the need for an instruction decoder
  and keeps hardware implementations simple.
- Consistent bit positions for register fields and right-aligned immediates.
- A small instruction set covering loads and stores, arithmetic, logic, shifts,
  branches, and jumps.

```{note}
Exact instruction formats and immediate ranges are in the
[specifications](https://gitlab.com/verify-at-iitpkd/gaur/-/tree/main/specifications).
```

### Example program

Computes the first `n` Fibonacci numbers and stores them in RAM.

```text
# Let RAM[0] store n and RAM[1] store k.
ld   r1 r0 0
ld   r2 r0 1
addi r3 r0 0
addi r5 r0 0
addi r6 r0 1
loop:
    beq  r3 r1 halt
    add  r4 r2 r3
    st   r5 r4 0
    add  r6 r6 r5
    sub  r5 r6 r5
    addi r3 r3 1
    j    loop
halt:
    j halt
```

## The VM language

The VM is stack-based and inspired by Nand2Tetris' Jack VM.

- **Segments**: `temp`, `static`, `argument`, `local`, plus the read-only
  `constant` pseudo-segment.
- **Instructions**: stack ops, arithmetic, relational and logical operators,
  control flow, and function calls.
- **Screen**: an 80×24 character display.
- **Standard library**: `Math` (`mul`, `div`, `mod`, `pow`, ...) and
  `Screen` (`print_char`, `print_int`, `newline`, ...).

## Project

Gaur is developed at IIT Palakkad by Rajdeep and M. Sreenath Karthick, under
the guidance of Dr. Piyush P. Kurur, as part of the systems and verification
group (`verify-at-iitpkd`).