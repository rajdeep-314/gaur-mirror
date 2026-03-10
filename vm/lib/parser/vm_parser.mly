(*
    vm_parser.mly

    Parser specification for Menhir for VM
*)

%{
    open Syntax.Ast 
%}

%token CONSTANT TEMP STATIC LOCAL ARGUMENT 
%token ADD SUB NEG EQ GT LT AND OR NOT
%token POP PUSH LABEL GOTO IFGOTO FUNCTION CALL RETURN EOF
%token <string> ID 
%token <int> NUM

%type <segment> segment
%type <(string, string) inst> inst
%type <(string, string) func> func
%type <(string, string) func list> funcs
%type <(string, string) prog> prog

%start prog

%%

segment:
    | CONSTANT    { Constant }
    | TEMP        { Temp }
    | STATIC      { Static }
    | LOCAL       { Local }
    | ARGUMENT    { Argument }

inst:
    | PUSH; s=segment; n=NUM       { Push (s, n) }
    | POP; s=segment; n=NUM        { Pop (s,n) }
    | ADD                          { Add }
    | SUB                          { Sub }
    | NEG                          { Neg }
    | EQ                           { Eq }
    | GT                           { Gt }
    | LT                           { Lt }
    | AND                          { And }
    | OR                           { Or }
    | NOT                          { Not }
    | LABEL; l=ID                  { Label l }
    | GOTO; l=ID                   { Goto l }
    | IFGOTO; l=ID                 { IfGoto l }
    | CALL; f=ID; n=NUM            { Call (f,n) }
    | RETURN                       { Return }

insts:
    | is=list(inst)     { is }

func:
    | FUNCTION; fn = ID; nVars = NUM; is = insts
        { {f_name = fn; n_vars = nVars; body = is} }

funcs:
    | fs=list(func)     { fs }

prog:
    | fs=funcs; EOF     { fs }

