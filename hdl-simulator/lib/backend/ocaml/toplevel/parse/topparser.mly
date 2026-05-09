%{
  open Types
  open Ast
  open Errors

  let add_to_map map (kvl : (string * Loc.t expr_info) Loc.wrapped) =
    let module StrMap = Stdlib.Map.Make(String) in
    let key = fst (kvl.txt) in
    let value = snd (kvl.txt) in
    let loc = kvl.loc in
    if StrMap.mem key map then
      raise_duplicate_key key loc
    else
      StrMap.add key value map
%}

%token <string>  LID
%token <Ast.bv> BV

(* commands *)
%token INPUT
%token OUTPUT
%token CLOCK
%token QUIT

(* operators *)
%token COMMA
%token FIELD_ASSIGN

(* parentheses *)
%token LBRACE RBRACE

%token EOF

%start <Loc.t Ast.cmd> cmd

%type <Loc.t Ast.expr_info> expr

%%

let cmd :=
  | INPUT; e = expr; EOF;
    { Input e }
  | OUTPUT;
    { Output }
  | CLOCK;
    { Clock }
  | QUIT;
    { Quit }

let expr :=
  | bv = BV;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Const bv) loc
    }
  | LBRACE; body = expr_rbody; RBRACE;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Record body) loc
    }


let expr_rbody :=
  | flist = separated_nonempty_list(COMMA, expr_rfield);
    { List.fold_left
        add_to_map
        StrMap.empty
        flist
    }

let expr_rfield :=
  | field = LID; FIELD_ASSIGN; expr = expr;
    { let loc = Loc.make ($startpos(field)) ($endpos(field)) in
      Loc.wrap ~loc:loc (field, expr)
    }
