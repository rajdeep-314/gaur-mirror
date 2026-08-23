(*
    types.ml
*)

(** {1 Types} *)

(**
This module contains OCaml types for PISA and PASM instructinos and programs.
The Rocq types in the project's theories are mapped to these using an
extraction script.
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


(** ISA---base instructions and programs. *)
module Isa = struct

    (** PISA instructions, without labels. *)
    type inst =
        | Li    of  reg * int
        | Ld    of  reg * reg * int
        | St    of  reg * reg * int
        | Add   of  reg * reg * reg
        | Addi  of  reg * reg * int
        | Sub   of  reg * reg * reg
        | And   of  reg * reg * reg
        | Not   of  reg * reg
        | Sl    of  reg * reg * int
        | Sr    of  reg * reg * int
        | Beq   of  reg * reg * int
        | Blt   of  reg * reg * int
        | Jalr  of  reg * reg * int
        | Apc   of  reg * int
        | J     of  int

    (** PISA programs. *)
    type prog = inst list

    (** Types with arbitrary annotations. *)

    (** Instructions with annotations of type 'a. *)
    type 'a annot_inst = inst * 'a
    (** Programs with annotations of type 'a. *)
    type 'a annot_prog = 'a annot_inst list

    (** Types with syntactic positions as annotations. *)
    type syn_inst = (Lexing.position * Lexing.position) annot_inst
    type syn_prog = (Lexing.position * Lexing.position) annot_prog

    (** Remove annotations from instructions and programs. *)
    let remove_annot_from_inst (i:'a annot_inst) : inst = fst i
    let remove_annot_from_prog (p:'a annot_prog) : prog = List.map fst p
end


(** Asm---assembly instructions and programs. *)
module Asm = struct

    (** PASM instructions with labels of type ['l]. *)
    type 'l inst =
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
    type 'l prog = 'l inst list
end


(** Machine programs *)
module Machine = struct
    (** Machine instructions. *)
    type inst = bool list

    (** Machine programs. *)
    type prog = inst list
end


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
    module Asm = struct
        open Asm

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
    end

    (** PISA instructions. *)
    module Isa = struct
        open Isa

        let li rd n = Li (rd,n)
        let ld rd rs1 n = Ld (rd, rs1, n)
        let st rd rs1 n = St (rd, rs1, n)
        let add rd rs1 rs2 = Add (rd, rs1, rs2)
        let addi rd rs1 n = Addi (rd, rs1, n)
        let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
        let and' rd rs1 rs2 = And (rd, rs1, rs2)
        let not rd rs1 = Not (rd, rs1)
        let sl rd rs n  = Sl (rd,rs,n)
        let sr rd rs n  = Sr (rd,rs,n)
        let beq rs1 rs2 l = Beq (rs1, rs2, l)
        let blt rs1 rs2 l = Blt (rs1, rs2, l)
        let jalr rd rs1 n = Jalr (rd, rs1, n)
        let apc rd n = Apc (rd, n)
        let j l = J l
    end
end

