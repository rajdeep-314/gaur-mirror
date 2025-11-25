type reg =
    | R0 | R1 | R2 | R3
    | R4 | R5 | R6 | R7


type 'l asm_inst =
    | Lli   of  reg * int
    | Lui   of  reg * int
    | Ld    of  reg * reg * imm
    | St    of  reg * reg * imm
    | Add   of  reg * reg * reg
    | Addi  of  reg * reg * imm
    | Sub   of  reg * reg * reg
    | And   of  reg * reg * reg
    | Not   of  reg * reg
    | Beq   of  reg * reg * 'l
    | Blt   of  reg * reg * 'l
    | Jalr  of  reg * reg * imm
    | Apc   of  reg * imm
    | J     of  'l
    | Label of  'l
    | Spin


type 'l asm_prog = ('l asm_inst) list


