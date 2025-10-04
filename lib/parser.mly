%{
  open Ast

  let rec separated_list sep f lexbuf =
  match f lexbuf with
  | x ->
    try
      match%lex lexbuf with
      | sep -> x :: separated_list sep f lexbuf
      | _ -> [x]
    with End_of_file -> [x]

    let int_to_bits = 0
%}

%token <string> STRING
%token <int>    INT
%token BITS

%token CHIP
%token INPUT
%token OUTPUT
%token STATES

%token PLUSPLUS
%token ELLIPSIS

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK

%token DEF
%token COLON
%token COMMA
%token PRIME

%token EOF

%start <Ast.prog> main
%%

main:
  | EOF       { [] }
  | prog EOF  { $1 }
  ;

prog:
  | chip      { [$1] }
  | chip prog { $1:$2 }
  ;

chip:
  | CHIP; STRING; LEFT_BRACE; in; out; states; assigns; RIGHT_BRACE
    { {
      name   = $2;
      input  = $4;
      output = $5;
      states = $6;
      parts  = $7
    } }
  ;

in:
  INPUT; name = STRING; COLON; t = type   { (name, t) }
  ;

out:
  OUTPUT; name = STRING; COLON; t = type  { (name, t) }
  ;

states:
  STATES; 
  st_list = separated_list(COMMA, state)  { st_list }
  ;

state:
  name = STRING; COLON; t = type          { (name, t) }
  ;

type:
  | BITS; i = INT                         { TBits i }
  | r = separated_list(COLON, trecord)    { TRecord r }
  ;

trecord:
  | name = STRING; COLON; t = type        { (name, t) }
  ;

assigns:
  | a = assign                { [a] }
  | a = assign; as = assigns  { a:as }
  ;

assign:
  | r = ref; DEF; e = expr                { Current(r, e) }
  | r = ref; PRIME; DEF; e = expr         { Next(r, e) }
  ;

ref:
  | f = STRING rest = ref_dots { 
      match rest with
      | None -> Field f
      | Some r -> Dot(f, r)
    }

ref_dots:
  | DOT; f = STRING; rest = ref_dots  
    { Some(Dot(f, rest |> Option.value ~default:(Field f))) }
  | 
    { None }

expr:
  | i = INT                               { Constant (int_to_bits i) }
  | r = reg                               { Access r }
  | name = STRING; e = expr               { Instance (name, expr) }
  | r = erecord                           { ERecord r }
  | e1 = expr; PLUSPLUS; e2 = expr        { Concat (e1, e2) }
  | e = expr; range;                      { ESplice (e,r ) }
  ;

range:
  | LEFT_PAREN; f = INT; ELLIPSIS; l = INT; RIGHT_PAREN
    { (f, l) }
  ;
