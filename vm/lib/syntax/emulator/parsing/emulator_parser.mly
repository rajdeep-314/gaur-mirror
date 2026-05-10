(*
    emulator_parser.mly

    Parser specification for Menhir for VM emulator programs.
*)

%{
    open Ast
    open Err.Helper.Parser

    let syscall_of_iden = function
        | "putchar" -> PutChar
        | s         -> raise (invalid_syscall s)
%}

%token CONSTANT TEMP STATIC LOCAL ARGUMENT
%token ADD SUB NEG EQ GT LT AND OR NOT
%token POP PUSH LABEL GOTO IFGOTO FUNCTION CALL SCALL RETURN EOF
%token REQ RLT RGT RLE RGE RNE
%token BADD BSUB BMUL BDIV
%token BLAND BLOR
%token STACK LPAREN RPAREN LSQUARE RSQUARE
%token TRUE FALSE UNOT
%token LBPT CBPT
%token <string> ID
%token <int> NUM

%type <segment>         segment
%type <syscall>         syscall
%type <relop>           relop
%type <binop>           binop
%type <boolop>          boolop
%type <exp>             exp
%type <cond_exp>        cond_exp
%type <def_inst>        inst
%type <def_func>        func
%type <def_func list>   funcs
%type <def_prog>        prog

%start prog


%left BLOR
%left BLAND
%left BADD BSUB
%left BMUL BDIV
%nonassoc UMINUS UNOT


%%

segment:
    | TEMP        { Temp }
    | STATIC      { Static }
    | LOCAL       { Local }
    | ARGUMENT    { Argument }

syscall:
    | f=ID        { syscall_of_iden f }

%inline relop:
    | REQ   { R_eq }
    | RLT   { R_lt }
    | RGT   { R_gt }
    | RLE   { R_le }
    | RGE   { R_ge }
    | RNE   { R_ne }

%inline binop:
    | BADD  { B_add }
    | BSUB  { B_sub }
    | BMUL  { B_mul }
    | BDIV  { B_div }

%inline boolop:
    | BLAND { Bl_and }
    | BLOR  { Bl_or }

exp:
    | n=NUM                                 { Const n }
    | STACK; LSQUARE; n=NUM; RSQUARE        { StackOffset n }
    | s=segment; LSQUARE; n=NUM; RSQUARE    { SegmentOffset (s, n) }
    | e1=exp; o=binop; e2=exp               { BinOp (o, e1, e2) }
    | BSUB; e=exp       %prec UMINUS        { Neg e }
    | LPAREN; e=exp; RPAREN                 { e }

cond_exp:
    | TRUE                                  { True }
    | FALSE                                 { False }
    | e1=exp; o=relop; e2=exp               { Comp (o, e1, e2) }
    | e1=cond_exp; o=boolop; e2=cond_exp    { BoolOp (o, e1, e2) }
    | UNOT; e=cond_exp                      { Not e }
    | LPAREN; e=cond_exp; RPAREN            { e }

inst:
    | PUSH; s=segment; n=NUM        { Push (s, n), ($startpos, $endpos) }
    | POP; s=segment; n=NUM         { Pop (s,n), ($startpos, $endpos) }
    | PUSH; CONSTANT; n=NUM         { PushC n, ($startpos, $endpos) }
    | ADD                           { Add, ($startpos, $endpos) }
    | SUB                           { Sub, ($startpos, $endpos) }
    | NEG                           { Neg, ($startpos, $endpos) }
    | EQ                            { Eq, ($startpos, $endpos) }
    | GT                            { Gt, ($startpos, $endpos) }
    | LT                            { Lt, ($startpos, $endpos) }
    | AND                           { And, ($startpos, $endpos) }
    | OR                            { Or, ($startpos, $endpos) }
    | NOT                           { Not, ($startpos, $endpos) }
    | LABEL; l=ID                   { Label l, ($startpos, $endpos) }
    | GOTO; l=ID                    { Goto l, ($startpos, $endpos) }
    | IFGOTO; l=ID                  { IfGoto l, ($startpos, $endpos) }
    | CALL; f=ID; n=NUM             { Call (f,n), ($startpos, $endpos) }
    | SCALL; f=syscall              { SCall f, ($startpos, $endpos) }
    | RETURN                        { Return, ($startpos, $endpos) }
    | LBPT; s=ID                    { LineBpt (s, None), ($startpos, $endpos) }
    | LBPT; s=ID; LPAREN; o=relop; n=NUM; RPAREN
        { LineBpt (s, Some (o, n)), ($startpos, $endpos) }
    | CBPT; s=ID; LPAREN; e=cond_exp; RPAREN
        { CondBpt (s, e), ($startpos, $endpos) }

insts:
    | is=list(inst)     { is }

func:
    | FUNCTION; fn = ID; nVars = NUM; is = insts
        { {f_name = fn; n_vars = nVars; body = is; annot = ($startpos, $endpos)} }

funcs:
    | fs=list(func)     { fs }

prog:
    | fs=funcs; EOF     { fs }

