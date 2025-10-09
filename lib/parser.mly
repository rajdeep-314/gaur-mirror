%{
  open Ast

  let rec int_to_bits n =
  if n = 0 then []
  else
    let b = if n mod 2 = 0 then O else I in
    b :: int_to_bits (n / 2)
%}

%token <string> LID (* lowercase identifier *)
%token <string> PID (* PascalCase identifier *)
%token <int>    INT (* integer literal *)

%token BITS CHIP INPUT OUTPUT STATES

%token FIELD_ASSIGN "="
%token DOT          "."
%token COLON        ":"
%token COMMA        ","

%token LBRACE "{" RBRACE "}" 
%token LPAREN "(" RPAREN ")"
%token LSQURE "[" RSQURE "]"

%token PLUSPLUS    "++"
%token PRIME       "'"
%token COMB_DEF    ":="
%token SEQ_DEF     "<-"

%token EOF

%left  PLUSPLUS
%left  DOT
%left  LSQURE
%left  APP

%start <Ast.program> main

%%

main:
  // | EOF
  //   { [] }
  | cs = chip* EOF
    { cs }

chip:
  | CHIP name = PID "{" body = chip_body "}"
    { { body with Ast.name = name } }

chip_body:
  | ind = in_decl outd = out_decl sts = states_decl? asgn = assign*
    { Ast.{
      name   = "";
      input  = ind;
      output = outd;
      states = sts;
      parts  = asgn;
    } }

in_decl:
  | INPUT id = LID ":" t = typ              { (id, t) }

out_decl:
  | OUTPUT id = LID ":" t = typ             { (id, t) }

states_decl:
  | STATES ss = separated_nonempty_list(",", state) { ss }

state:
  | id = LID ":" t = typ                    { (id, t) }

typ:
  | BITS n = INT                            { TBits n }
  | "{" r = record_t "}"                    { TRecord r }

record_t:
  | r = separated_nonempty_list(",", record_t_fields) { r }

record_t_fields:
  | field = LID ":" t = typ                 { (field, t) }

expression:
  | i = INT                                 { Constant (int_to_bits i) }
  | "(" expr = expression ")"               { expr }
  | expr = expression "." field = LID       { Member (expr, field) }
  | expr1 = expression "++" expr2 = expression
                                            { Concat (expr1, expr2) }
  | expr = expression "[" start_index = INT ":" end_index = INT "]" 
                                            { Splice (expr, (start_index, end_index)) }
  | expr = expression "[" i = INT "]"       { Splice (expr, (i, i)) }
  | "{" fs = record_fields "}"              { Record fs }
  | chip = PID expr = expression            { Instance (chip, expr) } %prec APP
  | id = LID                                { Access id }

record_fields:
  | separated_nonempty_list(",", record_field) { $1 }

record_field:
  | field = LID "=" expr = expression       { (field, expr) }

assign:
  | id = LID     ":=" expr = expression     { Current (id, expr) }
  | id = LID "'" "<-" expr = expression     { Next    (id, expr) }
