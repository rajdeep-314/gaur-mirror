(*
    vm_parser.mly

    Parser specification for Menhir for VM
*)

%{
    open Syntax.Ast 

    let syscall_of_iden = function
        | "putchar" -> PutChar
        | s         -> failwith ("Invalid syscall " ^ s)
%}

%token CONSTANT TEMP STATIC SCREEN LOCAL ARGUMENT
%token ADD SUB NEG EQ GT LT AND OR NOT
%token POP PUSH LABEL GOTO IFGOTO FUNCTION CALL SCALL RETURN EOF
%token <string> ID 
%token <int> NUM

%type <segment> segment
%type <syscall> syscall
%type <(string, string) inst> inst
%type <(string, string) func> func
%type <(string, string) func list> funcs
%type <(string, string) prog> prog

%start prog

%%

segment:
    | TEMP        { Temp }
    | STATIC      { Static }
    | SCREEN      { Screen }
    | LOCAL       { Local }
    | ARGUMENT    { Argument }

syscall:
    | f=ID        { syscall_of_iden f }

inst:
    | PUSH; s=segment; n=NUM       { Push (s, n) }
    | POP; s=segment; n=NUM        { Pop (s,n) }
    | PUSH; CONSTANT; n=NUM        { PushC n }
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
    | SCALL; f=syscall; n=NUM      { SCall (f,n) }
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

