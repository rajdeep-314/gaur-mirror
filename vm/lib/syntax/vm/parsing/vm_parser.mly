(*
    vm_parser.mly

    Parser specification for Menhir for VM
*)

%{
    module W = Types.Word

    open Ast
    open Err.Helper.Parser

    let syscall_of_iden f ps =
        match f with
        | "putchar" -> PutChar
        | s         -> raise (invalid_syscall s ps)

    (** [word_of_int n b ps] attempts to convert int [n] to [W.t], and raises
        a parsing exception if it is unable to do so -- an exception for
        [PushC] statements. [ps] is of type [Lexing.position * Lexing.position]
        and it corresponds to the offending positions. *)
    let word_of_int n ps =
        match W.of_int_opt n with
        | Some n -> n
        | None   -> raise (oob_pushc n ps)
%}

%token CONSTANT TEMP STATIC LOCAL ARGUMENT
%token ADD SUB NEG EQ GT LT AND OR NOT
%token POP PUSH LABEL GOTO IFGOTO FUNCTION CALL SCALL RETURN EOF
%token <string> ID
%token <int> NUM

%type <segment> segment
%type <syscall> syscall
%type <def_inst> inst
%type <def_func> func
%type <def_func list> funcs
%type <def_prog> prog

%start prog

%%

segment:
    | TEMP        { Temp }
    | STATIC      { Static }
    | LOCAL       { Local }
    | ARGUMENT    { Argument }

syscall:
    | f=ID        { syscall_of_iden f ($startpos(f), $endpos(f)) }

inst:
    | PUSH; s=segment; n=NUM       { Push (s, n), ($startpos, $endpos) }
    | POP; s=segment; n=NUM        { Pop (s,n), ($startpos, $endpos) }
    | PUSH; CONSTANT; n=NUM        { PushC (word_of_int n ($startpos(n), $endpos(n))), ($startpos, $endpos) }
    | ADD                          { Add, ($startpos, $endpos) }
    | SUB                          { Sub, ($startpos, $endpos) }
    | NEG                          { Neg, ($startpos, $endpos) }
    | EQ                           { Eq, ($startpos, $endpos) }
    | GT                           { Gt, ($startpos, $endpos) }
    | LT                           { Lt, ($startpos, $endpos) }
    | AND                          { And, ($startpos, $endpos) }
    | OR                           { Or, ($startpos, $endpos) }
    | NOT                          { Not, ($startpos, $endpos) }
    | LABEL; l=ID                  { Label l, ($startpos, $endpos) }
    | GOTO; l=ID                   { Goto l, ($startpos, $endpos) }
    | IFGOTO; l=ID                 { IfGoto l, ($startpos, $endpos) }
    | CALL; f=ID; n=NUM            { Call (f,n), ($startpos, $endpos) }
    | SCALL; f=syscall             { SCall f, ($startpos, $endpos) }
    | RETURN                       { Return, ($startpos, $endpos) }

insts:
    | is=list(inst)     { is }

func:
    | FUNCTION; fn = ID; nVars = NUM; is = insts
        { {f_name = fn; n_vars = nVars; body = is; annot = ($startpos, $endpos)} }

funcs:
    | fs=list(func)     { fs }

prog:
    | fs=funcs; EOF     { fs }

