(*
    parser.mly

    Parser specification for Menhir.
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
       JALR APC J SPIN COLON NEWLINE EOF
%token <string> LABEL
%token <int> REG IMM

%type <Types.reg> reg
%type <int> imm
%type <string> lbl
%type <string Types.asm_inst> inst
%type <string Types.asm_inst list> stmt
%type <string Types.asm_prog> prog

%start prog


%%


reg:
    | i = REG   { reg_of_int i }

imm:
    | n = IMM   { n }

lbl:
    | l = LABEL { l }

inst:
    | LLI; r = reg; n = imm                 { lli r n }
    | LUI; r = reg; n = imm                 { lui r n }
    | LD; rd = reg; rs1 = reg; n = imm      { ld rd rs1 n }
    | ST; rd = reg; rs1 = reg; n = imm      { st rd rs1 n }
    | ADD; rd = reg; rs1 = reg; rs2 = reg   { add rd rs1 rs2 }
    | ADDI; rd = reg; rs1 = reg; n = imm    { addi rd rs1 n }
    | SUB; rd = reg; rs1 = reg; rs2 = reg   { sub rd rs1 rs2 }
    | AND; rd = reg; rs1 = reg; rs2 = reg   { and' rd rs1 rs2 }
    | NOT; rd = reg; rs1 = reg              { not rd rs1 }
    | BEQ; rs1 = reg; rs2 = reg; l = lbl    { beq rs1 rs2 l }
    | BLT; rs1 = reg; rs2 = reg; l = lbl    { blt rs1 rs2 l }
    | JALR; rd = reg; rs1 = reg; n = imm    { jalr rd rs1 n }
    | APC; rd = reg; n = imm                { apc rd n }
    | J; l = lbl                            { j l }
    | l = lbl; COLON                        { label l }
    | SPIN                                  { spin }

stmt:
    | i = inst; NEWLINE                     { [i] }
    | i = inst; EOF                         { [i] }
    | NEWLINE                               { [] }

prog:
    | EOF                                   { [] }
    | s = stmt; p = prog                    { s @ p }

