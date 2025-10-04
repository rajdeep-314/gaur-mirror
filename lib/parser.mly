%{
  open Ast

  let int_to_bits = 65535
%}

%token <string> IDENT
%token <int>    INT
%token BITS CHIP INPUT OUTPUT STATES
%token EQ DOT COLON COMMA LEFT_BRACE RIGHT_BRACE LEFT_PAREN RIGHT_PAREN LEFT_BRACK RIGHT_BRACK
%token PLUSPLUS ELLIPSIS PRIME DEF
%token EOF

%left PLUSPLUS
%left DOT
%left LEFT_BRACK
%right APP

%start <Ast.program> main
%%

// main rules
main:
  | EOF       { [] }
  | prog EOF  { $1 }
  ;

prog:
  | chip      { [$1] }
  | chip prog { $1:$2 }
  ;

// chip declarations
chip:
  | CHIP IDENT LEFT_BRACE chip_body RIGHT_BRACE
    { $4 with Ast.name = $2 }
  ;

chip_body:
  | in_decl out_decl opt_states_decl assigns
    { Ast.{
      name = "";
      input = $1;
      output = $2;
      states = $3;
      parts = $4
    } }
  ;

in_decl:
  | INPUT IDENT COLON type      { ($2, $4) }
  ;

out_decl:
  | OUTPUT IDENT COLON type     { ($2, $4) }
  ;

opt_states_decl:
  | STATES states_list          { $2 }
  |                             { [] }
  ;

state_list:
  | state                       { [$1] }
  | state COMMA state_list      { $1 :: $3 }

state:
  IDENT COLON type              { ($1, $3) }
  ;

// types
type:
  | BITS INT                    { TBits $2 }
  | LEFT_BRACE r_fields RIGHT_BRACE
                                { TRecord $2 }
  ;

r_fields:
  | r_field                     { [$1] }
  | r_field COMMA r_fields      { $1 :: $3 }
  ;

r_field:
  | IDENT COLON type            { ($1, $3) }
  ;

// expressions
expr:
  | INT                         { Constant (int_to_bits $1) }
  | LEFT_PAREN expr RIGHT_PAREN { $2 }
  | expr DOT IDENT              { EAccess ($1, $3) }
  | expr PLUSPLUS expr          { Concat ($1, $3) }
  | expr LEFT_BRACK INT ELLIPSIS INT RIGHT_BRACK
                                { ESplice ($1, ($3, $5)) }
  | LEFT_BRACE expr_fields RIGHT_BRACE
                                { ERecord $2 }
  | IDENT expr                  { Instance ($1, $2) } %prec APP
  ;

expr_fields:
  | expr_field                  { [$1] }
  | expr_field COMMA expr_fields
                                { $1 :: $3 }
  ;

expr_field:
  | IDENT EQ expr               { ($1, $3) }
  ;

// patterns
pattern:
  | IDENT                       { Variable $1 }
  | pattern DOT IDENT           { PAccess ($1, $3) }
  | pattern LEFT_BRACK INT ELLIPSIS INT RIGHT_BRACK
                                { PSplice ($1, ($3, $5)) }
  ;

// assignments
assigns:
  |                             { [] }
  | assign assigns              { $1 :: $2 }
  ;

assign:
  | pattern DEF expr            { Current ($1, $3) }
  | pattern PRIME DEF expr      { Next ($1, $4) }
  ;
