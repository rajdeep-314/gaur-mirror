(*
    parser_isa.mly

    Parser specification for Menhir for PISA.
*)


%{
    open Types
    open Types.Helper

    let reg_of_int = function
        | 0    -> R0  | 1    -> R1  | 2    -> R2  | 3    -> R3
        | 4    -> R4  | 5    -> R5  | 6    -> R6  | 7    -> R7
        | 8    -> R8  | 9    -> R9  | 10   -> R10 | 11   -> R11
        | 12   -> R12 | 13   -> R13 | 14   -> R14 | 15   -> R15
        | _     -> failwith "Invalid argument to reg_of_int."
%}


%token LLI LUI LD ST ADD ADDI SUB AND NOT BEQ BLT
       JALR APC J NEWLINE EOF
%token <int> REG IMM

%type <Types.reg> reg
%type <int> imm
%type <Types.isa_inst> inst
%type <Types.isa_inst list> stmt
%type <Types.isa_prog> prog

%start prog


%%


reg:
    | i = REG   { reg_of_int i }

imm:
    | n = IMM   { n }

inst:
    | LLI; r = reg; n = imm                 { illi r n }
    | LUI; r = reg; n = imm                 { ilui r n }
    | LD; rd = reg; rs1 = reg; n = imm      { ild rd rs1 n }
    | ST; rd = reg; rs1 = reg; n = imm      { ist rd rs1 n }
    | ADD; rd = reg; rs1 = reg; rs2 = reg   { iadd rd rs1 rs2 }
    | ADDI; rd = reg; rs1 = reg; n = imm    { iaddi rd rs1 n }
    | SUB; rd = reg; rs1 = reg; rs2 = reg   { isub rd rs1 rs2 }
    | AND; rd = reg; rs1 = reg; rs2 = reg   { iand' rd rs1 rs2 }
    | NOT; rd = reg; rs1 = reg              { inot rd rs1 }
    | BEQ; rs1 = reg; rs2 = reg; n = imm    { ibeq rs1 rs2 n }
    | BLT; rs1 = reg; rs2 = reg; n = imm    { iblt rs1 rs2 n }
    | JALR; rd = reg; rs1 = reg; n = imm    { ijalr rd rs1 n }
    | APC; rd = reg; n = imm                { iapc rd n }
    | J; n = imm                            { ij n }

stmt:
    | i = inst; NEWLINE                     { [i] }
    | i = inst; EOF                         { [i] }
    | NEWLINE                               { [] }

prog:
    | EOF                                   { [] }
    | s = stmt; p = prog                    { s @ p }

