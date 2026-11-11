(*
    parser_asm.mly

    Parser specification for Menhir for Gaur ASM.
*)


%{
    open Extracted.Reg
    open Extracted.Asm
    open Extracted.Arith
    open Extracted.Bv

    open Errors.Helper.ParsingExns

    (** [syn_inst] -- syntactic instructions with lexing info as annotations. *)
    type syn_inst = (string, Lexing.position * Lexing.position) annot_inst

    (** [syn_prog] -- syntactic programs with lexing info as annotations. *)
    type syn_prog = (string, Lexing.position * Lexing.position) annot_prog

    (** Register corresponding to index [n]. *)
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

    (** 11-bit unsigned immediate corresponding to int [n]. *)
    let imm11u_of_int n ps =
        let (lb, ub) = po2_unsigned_range 11 in
        if lb <= n && n <= ub then of_Z n 11
        else oobimm11u n ps

    (** 11-bit signed immediate corresponding to int [n]. *)
    let imm11s_of_int n ps =
        let (lb, ub) = po2_signed_range 11 in
        if lb <= n && n <= ub then of_Z n 11
        else oobimm11s n ps

    (** 6-bit signed immediate corresponding to int [n]. *)
    let imm6s_of_int n ps =
        let (lb, ub) = po2_signed_range 6 in
        if lb <= n && n <= ub then of_Z n 6
        else oobimm6s n ps

    (** 5-bit unsigned immediate corresponding to int [n]. *)
    let imm5u_of_int n ps =
        let (lb, ub) = po2_unsigned_range 5 in
        if lb <= n && n <= ub then of_Z n 5
        else oobimm5u n ps
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

%type <Extracted.Reg.t> reg
%type <Extracted.Bv.t> imm11u       (* 11-bit unsigned immediate *)
%type <Extracted.Bv.t> imm11s       (* 11-bit signed immediate   *)
%type <Extracted.Bv.t> imm6s        (* 6-bit signed immediate    *)
%type <Extracted.Bv.t> imm5u        (* 5-bit unsigned immediate  *)
%type <string> lbl
%type <(string, Lexing.position * Lexing.position) annot_inst> inst
%type <(string, Lexing.position * Lexing.position) annot_inst list> insts
%type <(string, Lexing.position * Lexing.position) annot_prog> prog

%start prog


%%


reg:
    | i = REG   { reg_of_int i ($startpos, $endpos) }

imm11u:
    | n = IMM   { imm11u_of_int n ($startpos, $endpos) }

imm11s:
    | n = IMM   { imm11s_of_int n ($startpos, $endpos) }

imm6s:
    | n = IMM   { imm6s_of_int n ($startpos, $endpos) }

imm5u:
    | n = IMM   { imm5u_of_int n ($startpos, $endpos) }

lbl:
    | l = LABEL { l }

inst:
    | LI; r = reg; n = imm11u
        { Li (r, n), ($startpos, $endpos) }
    | LD; rd = reg; rs1 = reg; n = imm6s
        { Ld (rd, rs1, n), ($startpos, $endpos) }
    | ST; rd = reg; rs1 = reg; n = imm6s
        { St (rd, rs1, n), ($startpos, $endpos) }
    | ADD; rd = reg; rs1 = reg; rs2 = reg
        { Add (rd, rs1, rs2), ($startpos, $endpos) }
    | ADDI; rd = reg; rs1 = reg; n = imm6s
        { Addi (rd, rs1, n), ($startpos, $endpos) }
    | SUB; rd = reg; rs1 = reg; rs2 = reg
        { Sub (rd, rs1, rs2), ($startpos, $endpos) }
    | AND; rd = reg; rs1 = reg; rs2 = reg
        { And (rd, rs1, rs2), ($startpos, $endpos) }
    | NOT; rd = reg; rs1 = reg
        { Not (rd, rs1), ($startpos, $endpos) }
    | SL; rd = reg; rs = reg ; n = imm5u
        { Sl (rd, rs, n), ($startpos, $endpos) }
    | SR; rd = reg; rs = reg ; n = imm5u
        { Sr (rd, rs, n), ($startpos, $endpos)} 
    | BEQ; rs1 = reg; rs2 = reg; l = lbl
        { Beq (rs1, rs2, l), ($startpos, $endpos) }
    | BLT; rs1 = reg; rs2 = reg; l = lbl
        { Blt (rs1, rs2, l), ($startpos, $endpos) }
    | JALR; rd = reg; rs1 = reg; n = imm6s
        { Jalr (rd, rs1, n), ($startpos, $endpos) }
    | APC; rd = reg; n = imm11s
        { Apc (rd, n), ($startpos, $endpos) }
    | J; l = lbl
        { J l, ($startpos, $endpos) }
    | l = lbl; COLON
        { Label l, ($startpos, $endpos) }
    | SPIN
        { Spin, ($startpos, $endpos) }

insts:
    | is=inst*                              { is }

prog:
    | is=insts; EOF                         { is }

