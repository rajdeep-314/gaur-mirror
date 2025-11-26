type reg =
    | R0 | R1 | R2 | R3
    | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | R11
    | R12 | R13 | R14 | R15

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

type 'l asm_prog = ('l asm_inst) list

type machine_inst = bool list
type machine_prog = machine_inst list

