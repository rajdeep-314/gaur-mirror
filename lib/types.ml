(* Registers. *)
type reg =
    | R0 | R1 | R2 | R3
    | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | R11
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

type isa_prog = isa_inst list                   (* ISA program. *)

type machine_inst = bool list                   (* Machine instruction. *)
type machine_prog = machine_inst list           (* Machine program.     *)

