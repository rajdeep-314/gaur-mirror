(*
    types.ml

    OCaml types for assembly instructions and programs, ISA instructuctions
    and programs, etc.
    The Rocq types are mapped to these in extraction.v
*)

(* Registers. *)
type reg =
    | R0  | R1  | R2  | R3
    | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15


(* PASM instructions with labels of type 'l. *)
type 'l asm_inst =
    | Lli   of  reg * int
    | Lui   of  reg * int
    | Ld    of  reg * reg * int
    | St    of  reg * reg * int
    | Add   of  reg * reg * reg
    | Addi  of  reg * reg * int
    | Sub   of  reg * reg * reg
    | And   of  reg * reg * reg
    | Not   of  reg * reg
    | Beq   of  reg * reg * 'l
    | Blt   of  reg * reg * 'l
    | Jalr  of  reg * reg * int
    | Apc   of  reg * int
    | J     of  'l
    | Label of  'l
    | Spin

(* PASM programs. *)
type 'l asm_prog = ('l asm_inst) list

(* ISA instructions. *)
type isa_inst =
    | I_lli   of  reg * int
    | I_lui   of  reg * int
    | I_ld    of  reg * reg * int
    | I_st    of  reg * reg * int
    | I_add   of  reg * reg * reg
    | I_addi  of  reg * reg * int
    | I_sub   of  reg * reg * reg
    | I_and   of  reg * reg * reg
    | I_not   of  reg * reg
    | I_beq   of  reg * reg * int
    | I_blt   of  reg * reg * int
    | I_jalr  of  reg * reg * int
    | I_apc   of  reg * int
    | I_j     of  int

type isa_prog = isa_inst list                   (* ISA programs. *)

type machine_inst = bool list                   (* Machine instructions. *)
type machine_prog = machine_inst list           (* Machine programs.     *)


(* Helper functions to write AST programs. *)
module Helper = struct
    (* Registers. *)
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

    (* PASM. *)
    let lli rd n = Lli (rd, n)
    let lui rd n = Lui (rd, n)
    let ld rd rs1 n = Ld (rd, rs1, n)
    let st rd rs1 n = St (rd, rs1, n)
    let add rd rs1 rs2 = Add (rd, rs1, rs2)
    let addi rd rs1 n = Addi (rd, rs1, n)
    let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
    let and' rd rs1 rs2 = And (rd, rs1, rs2)
    let not rd rs1 = Not (rd, rs1)
    let beq rs1 rs2 l = Beq (rs1, rs2, l)
    let blt rs1 rs2 l = Blt (rs1, rs2, l)
    let jalr rd rs1 n = Jalr (rd, rs1, n)
    let apc rd n = Apc (rd, n)
    let j l = J l
    let label l = Label l
    let spin = Spin

    (* PISA. *)
    let illi rd n = I_lli (rd, n)
    let ilui rd n = I_lui (rd, n)
    let ild rd rs1 n = I_ld (rd, rs1, n)
    let ist rd rs1 n = I_st (rd, rs1, n)
    let iadd rd rs1 rs2 = I_add (rd, rs1, rs2)
    let iaddi rd rs1 n = I_addi (rd, rs1, n)
    let isub rd rs1 rs2 = I_sub (rd, rs1, rs2)
    let iand' rd rs1 rs2 = I_and (rd, rs1, rs2)
    let inot rd rs1 = I_not (rd, rs1)
    let ibeq rs1 rs2 l = I_beq (rs1, rs2, l)
    let iblt rs1 rs2 l = I_blt (rs1, rs2, l)
    let ijalr rd rs1 n = I_jalr (rd, rs1, n)
    let iapc rd n = I_apc (rd, n)
    let ij l = I_j l
end

