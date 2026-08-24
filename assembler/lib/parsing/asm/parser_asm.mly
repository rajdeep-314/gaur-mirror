(*
    parser_asm.mly

    Parser specification for Menhir for PASM.
*)


%{
    open Errors.Helper.ParsingExns
    open Types
    open Types.Helper.Asm

    let reg_of_int n ps =
        match n with
        | 0    -> R0  | 1    -> R1  | 2    -> R2  | 3    -> R3
        | 4    -> R4  | 5    -> R5  | 6    -> R6  | 7    -> R7
        | 8    -> R8  | 9    -> R9  | 10   -> R10 | 11   -> R11
        | 12   -> R12 | 13   -> R13 | 14   -> R14 | 15   -> R15
        | 16   -> R16 | 17   -> R17 | 18   -> R18 | 19   -> R19
        | 20   -> R20 | 21   -> R21 | 22   -> R22 | 23   -> R23
        | 24   -> R24 | 25   -> R25 | 26   -> R26 | 27   -> R27
        | 28   -> R28 | 29   -> R29 | 30   -> R30 | 31   -> R31
        | _    -> invalidregnum n ps
%}


(* Instructions. *)
%token LI
%token LD ST
%token ADD ADDI SUB
%token AND NOT
%token SL SR
%token BEQ BLT
%token JALR APC J

(* Pseudo-instruction(s). *)
%token SPIN

(* Colon (for labels) and end-of-file token. *)
%token COLON EOF

(* Labels (strings), registers and immediates. *)
%token <string> LABEL
%token <int> REG IMM

%type <Types.reg> reg
%type <int> imm
%type <string> lbl
%type <Types.Asm.syn_inst> inst
%type <Types.Asm.syn_inst list> insts
%type <Types.Asm.syn_prog> prog

%start prog


%%


reg:
    | i = REG   { reg_of_int i ($startpos, $endpos) }

imm:
    | n = IMM   { n }

lbl:
    | l = LABEL { l }

inst:
    | LI; r = reg; n = imm
        { li r n, ($startpos, $endpos) }
    | LD; rd = reg; rs1 = reg; n = imm
        { ld rd rs1 n, ($startpos, $endpos) }
    | ST; rd = reg; rs1 = reg; n = imm
        { st rd rs1 n, ($startpos, $endpos) }
    | ADD; rd = reg; rs1 = reg; rs2 = reg
        { add rd rs1 rs2, ($startpos, $endpos) }
    | ADDI; rd = reg; rs1 = reg; n = imm
        { addi rd rs1 n, ($startpos, $endpos) }
    | SUB; rd = reg; rs1 = reg; rs2 = reg
        { sub rd rs1 rs2, ($startpos, $endpos) }
    | AND; rd = reg; rs1 = reg; rs2 = reg
        { and' rd rs1 rs2, ($startpos, $endpos) }
    | NOT; rd = reg; rs1 = reg
        { not rd rs1, ($startpos, $endpos) }
    | SL; rd = reg; rs = reg ; n = imm
        { sl rd rs n, ($startpos, $endpos) }
    | SR; rd = reg; rs = reg ; n = imm
        { sr rd rs n , ($startpos, $endpos)} 
    | BEQ; rs1 = reg; rs2 = reg; l = lbl
        { beq rs1 rs2 l, ($startpos, $endpos) }
    | BLT; rs1 = reg; rs2 = reg; l = lbl
        { blt rs1 rs2 l, ($startpos, $endpos) }
    | JALR; rd = reg; rs1 = reg; n = imm
        { jalr rd rs1 n, ($startpos, $endpos) }
    | APC; rd = reg; n = imm
        { apc rd n, ($startpos, $endpos) }
    | J; l = lbl
        { j l, ($startpos, $endpos) }
    | l = lbl; COLON
        { label l, ($startpos, $endpos) }
    | SPIN
        { spin, ($startpos, $endpos) }

insts:
    | is=inst*                              { is }

prog:
    | is=insts; EOF                         { is }

