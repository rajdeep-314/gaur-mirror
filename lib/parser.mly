%{
  open Ast

  let rec int_to_bits n =
  if n = 0 then []
  else
    let b = if n mod 2 = 0 then O else I in
    b :: int_to_bits (n / 2)
%}

%token <string> IDENT
%token <int>    INT
%token BITS CHIP INPUT OUTPUT STATES
%token EQ DOT COLON COMMA LEFT_BRACE RIGHT_BRACE LEFT_PAREN RIGHT_PAREN LEFT_BRACK RIGHT_BRACK
%token PLUSPLUS PRIME DEF
%token EOF

%left PLUSPLUS
%left DOT
%left LEFT_BRACK
%right IDENT
%left APP

%start <Ast.program> main
%%

// main rules
main:
  | EOF                         { [] }
  | nonempty_list(chip) EOF     { $1 }
  ;

// chip declarations
chip:
  | CHIP IDENT LEFT_BRACE chip_body RIGHT_BRACE
    { { $4 with Ast.name = $2 } }
  ;

chip_body:
  | in_decl out_decl option(states_decl) list(assign)
    { Ast.{
      name = "";
      input = $1;
      output = $2;
      states = $3;
      parts = $4
    } }
  ;

in_decl:
  | INPUT IDENT COLON typ       { ($2, $4) }
  ;

out_decl:
  | OUTPUT IDENT COLON typ      { ($2, $4) }
  ;

states_decl:
  | STATES separated_nonempty_list(COMMA, state)
                                { $2 }

state:
  IDENT COLON typ               { ($1, $3) }
  ;

// types
typ :
  | BITS INT                    { TBits $2 }
  | LEFT_BRACE r_fields RIGHT_BRACE
                                { TRecord $2 }
  ;

r_fields:
  | separated_nonempty_list(COMMA, r_field)
                                { $1 }
  ;

r_field:
  | IDENT COLON typ             { ($1, $3) }
  ;

// expressions
expr:
  | INT                         { Constant (int_to_bits $1) }
  | LEFT_PAREN expr RIGHT_PAREN { $2 }
  | expr DOT IDENT              { EAccess ($1, $3) }
  | expr PLUSPLUS expr          { Concat ($1, $3) }
  | expr LEFT_BRACK INT COLON INT RIGHT_BRACK
                                { ESplice ($1, ($3, $5)) }
  | LEFT_BRACE expr_fields RIGHT_BRACE
                                { ERecord $2 }
  | IDENT expr                  { Instance ($1, $2) } %prec APP
  | IDENT                       { EVariable $1 }
  ;

expr_fields:
  | separated_nonempty_list(COMMA, expr_field)
                                { $1 }
  ;

expr_field:
  | IDENT "=" expr               { ($1, $3) }
  ;

// patterns
pattern:
  | IDENT                       { PVariable $1 }
  | pattern DOT IDENT           { PAccess ($1, $3) }
  | pattern LEFT_BRACK INT COLON INT RIGHT_BRACK
                                { PSplice ($1, ($3, $5)) }
  ;

assign:
  | pattern DEF expr            { Current ($1, $3) }
  | pattern PRIME DEF expr      { Next ($1, $4) }
  ;
