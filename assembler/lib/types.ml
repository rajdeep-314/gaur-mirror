(*
    types.ml
*)

(** {1 Types} *)

(**
This module contains OCaml types for PISA and PASM instructinos and programs.
The Rocq types in the project's theories are mapped to these using an extraction script.
*)

(** Registers. *)
type reg =
    | R0  | R1  | R2  | R3
    | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15
    | R16 | R17 | R18 | R19
    | R20 | R21 | R22 | R23
    | R24 | R25 | R26 | R27
    | R28 | R29 | R30 | R31


(** PASM instructions with labels of type ['l]. *)
type 'l asm_inst =
    | Li    of  reg * int       (** Load immediate. *)
    | Ld    of  reg * reg * int (** Load. *)
    | St    of  reg * reg * int (** Store. *)
    | Add   of  reg * reg * reg (** Add two registers. *)
    | Addi  of  reg * reg * int (** Add register and immediate. *)
    | Sub   of  reg * reg * reg (** Subtract two registers. *)
    | And   of  reg * reg * reg (** Bitwise and between two registers. *)
    | Not   of  reg * reg       (** Bitwise negation of a register. *)
    | Sl    of  reg * reg * int (** Logical Shift Left. *)
    | Sr    of  reg * reg * int (** Logical Shift Right. *)
    | Beq   of  reg * reg * 'l  (** Branch if equal-to. *)
    | Blt   of  reg * reg * 'l  (** Branch if less-than. *)
    | Jalr  of  reg * reg * int (** Jump and link register. *)
    | Apc   of  reg * int       (** Add to PC and store in a register. *)
    | J     of  'l              (** (Unconditional) jump. *)
    | Label of  'l              (** Define a label. *)
    | Spin                      (** Spin endlessly. *)

(** PASM programs. *)
type 'l asm_prog = ('l asm_inst) list

(** PISA instructions, without labels. *)
type isa_inst =
    | I_li    of  reg * int
    | I_ld    of  reg * reg * int
    | I_st    of  reg * reg * int
    | I_add   of  reg * reg * reg
    | I_addi  of  reg * reg * int
    | I_sub   of  reg * reg * reg
    | I_and   of  reg * reg * reg
    | I_not   of  reg * reg
    | I_sl    of  reg * reg * int
    | I_sr    of  reg * reg * int
    | I_beq   of  reg * reg * int
    | I_blt   of  reg * reg * int
    | I_jalr  of  reg * reg * int
    | I_apc   of  reg * int
    | I_j     of  int

(** PISA programs. *)
type isa_prog = isa_inst list

(** Machine instructions. *)
type machine_inst = bool list

(** Machine programs. *)
type machine_prog = machine_inst list


(** Helper functions to write AST programs. *)
module Helper = struct
    (** Registers. *)

    let r0 = R0
    let r1 = R1
    let r2 = R2
    let r3 = R3
    let r4 = R4
    let r5 = R5
    let r6 = R6
    let r7 = R7
    let r8 = R8
    let r9 = R9
    let r10 = R10
    let r11 = R11
    let r12 = R12
    let r13 = R13
    let r14 = R14
    let r15 = R15
    let r16 = R16
    let r17 = R17
    let r18 = R18
    let r19 = R19
    let r20 = R20
    let r21 = R21
    let r22 = R22
    let r23 = R23
    let r24 = R24
    let r25 = R25
    let r26 = R26
    let r27 = R27
    let r28 = R28
    let r29 = R29
    let r30 = R30
    let r31 = R31

    (** PASM instructions. *)

    let li rd n = Li (rd,n)
    let ld rd rs1 n = Ld (rd, rs1, n)
    let st rd rs1 n = St (rd, rs1, n)
    let add rd rs1 rs2 = Add (rd, rs1, rs2)
    let addi rd rs1 n = Addi (rd, rs1, n)
    let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
    let and' rd rs1 rs2 = And (rd, rs1, rs2)
    let not rd rs1 = Not (rd, rs1)
    let sl rd rs n = Sl (rd,rs,n)
    let sr rd rs n  = Sr (rd,rs,n)
    let beq rs1 rs2 l = Beq (rs1, rs2, l)
    let blt rs1 rs2 l = Blt (rs1, rs2, l)
    let jalr rd rs1 n = Jalr (rd, rs1, n)
    let apc rd n = Apc (rd, n)
    let j l = J l
    let label l = Label l
    let spin = Spin

    (** PISA instructions. *)

    let ili rd n = I_li (rd,n)
    let ild rd rs1 n = I_ld (rd, rs1, n)
    let ist rd rs1 n = I_st (rd, rs1, n)
    let iadd rd rs1 rs2 = I_add (rd, rs1, rs2)
    let iaddi rd rs1 n = I_addi (rd, rs1, n)
    let isub rd rs1 rs2 = I_sub (rd, rs1, rs2)
    let iand rd rs1 rs2 = I_and (rd, rs1, rs2)
    let inot rd rs1 = I_not (rd, rs1)
    let isl rd rs n  = I_sl (rd,rs,n)
    let isr rd rs n  = I_sr (rd,rs,n)
    let ibeq rs1 rs2 l = I_beq (rs1, rs2, l)
    let iblt rs1 rs2 l = I_blt (rs1, rs2, l)
    let ijalr rd rs1 n = I_jalr (rd, rs1, n)
    let iapc rd n = I_apc (rd, n)
    let ij l = I_j l
end


