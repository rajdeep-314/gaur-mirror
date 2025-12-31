%{
    open Ast
%}


%token HELP SET_TRACE ON OFF GET_TRACE REG REGS MEM STEP STEPS PRINT_INST
       PRINT_PROG QUIT EOF
%token <int> NUM REGISTER


%type <Ast.t> inst
%type <Ast.t> line

%start line


%%


inst:
    | HELP                  { Help }
    | SET_TRACE; ON         { SetTrace true }
    | SET_TRACE; OFF        { SetTrace false }
    | GET_TRACE             { GetTrace }
    | REG; n = REGISTER     { Reg n }
    | REGS                  { Regs }
    | PRINT_INST            { Print_inst }
    | PRINT_PROG            { Print_prog }
    | MEM; a = NUM; n = NUM { Mem (a, n) }
    | STEP                  { Step }
    | STEPS n = NUM         { Steps n }
    | QUIT                  { Quit }

line:
    | i = inst; EOF         { i }

