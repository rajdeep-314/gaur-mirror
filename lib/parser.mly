%{
    open Ast
%}


%token <Big_int.big_int> NUM
%token COLON NEWLINE EOF

%type <int> addr
%type <Big_int.big_int> half
%type <Ast.t> map
%type <Ast.maps> prog

%start prog


%%


addr:
    | n = NUM       { Big_int.int_of_big_int n }

half:
    | n = NUM       { n }

map:
    | a = addr; COLON; h = half
        { Map (a, h) }

prog:
    | ms = list(terminated(map, NEWLINE*)); EOF 
        { ms }

