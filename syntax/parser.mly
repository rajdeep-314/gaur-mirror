%{
    open Ast
    open Ast.Helper
%}


%token LLI LUI LD ST ADD ADDI SUB AND NOT BEQ BLT
       JALR APC J NEWLINE EOF
%token <int> REG IMM

%type <Ast.Reg.t> reg
%type <Ast.t> inst
%type <Ast.prog> prog

%start prog


%%


reg:
    | i = REG   { Reg.of_int i }

inst:
    | LLI; r = reg; n = IMM                 { lli r n }
    | LUI; r = reg; n = IMM                 { lui r n }
    | LD; rd = reg; rs1 = reg; n = IMM      { ld rd rs1 n }
    | ST; rd = reg; rs1 = reg; n = IMM      { st rd rs1 n }
    | ADD; rd = reg; rs1 = reg; rs2 = reg   { add rd rs1 rs2 }
    | ADDI; rd = reg; rs1 = reg; n = IMM    { addi rd rs1 n }
    | SUB; rd = reg; rs1 = reg; rs2 = reg   { sub rd rs1 rs2 }
    | AND; rd = reg; rs1 = reg; rs2 = reg   { and_ rd rs1 rs2 }
    | NOT; rd = reg; rs1 = reg              { not rd rs1 }
    | BEQ; rs1 = reg; rs2 = reg; n = IMM    { beq rs1 rs2 n }
    | BLT; rs1 = reg; rs2 = reg; n = IMM    { blt rs1 rs2 n }
    | JALR; rd = reg; rs1 = reg; n = IMM    { jalr rd rs1 n }
    | APC; rd = reg; n = IMM                { apc rd n }
    | J; n = IMM                            { j n }

prog:
    | l = list(terminated(inst, NEWLINE*)); EOF
        { l }

