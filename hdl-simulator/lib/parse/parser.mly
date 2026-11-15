%{
  open Types
  open Ast
  open Hdlutils
  open Errors

  let add_to_map map (kvl : 'a Loc.wrapped) =
    let module StrMap = Stdlib.Map.Make(String) in
    let key = fst (kvl.txt) in
    let value = snd (kvl.txt) in
    let loc = kvl.loc in
    if StrMap.mem key map then
      raise (DuplicateKey (key, loc))
    else
      StrMap.add key value map

  let of_binary str : Bit.t NonEmpty.t =
    str
    |> String.to_seq
    |> Seq.map (function
       | '0' -> false
       | '1' -> true
       |  c  -> raise (LexerError   ( Printf.sprintf
                                      "Unexpected '%s'.\n"
                                      str)))
    |> List.of_seq
    |> NonEmpty.of_list
%}

%token <string> LID (* 'lowercase' identifier *)
%token <string> PID (* 'PascalCase' identifier *)
%token <string> INT

(* keywords *)
%token BITS CHIP INPUT OUTPUT STATES

(* operators *)
%token FIELD_ASSIGN
%token DOT
%token COLON
%token PLUSPLUS
%token PRIME
%token COMB_DEF
%token SEQ_DEF
%token COMMA

(* parentheses *)
%token LBRACE  RBRACE
%token LPAREN  RPAREN
%token LSQUARE RSQUARE

%token EOF

%left PLUSPLUS
%right DOT
%left LSQUARE

%start <Loc.t Ast.prog> prog

%type <Loc.t Ast.chip_info> chip
%type <Loc.t Ast.assign_info> assign
%type <Loc.t Ast.rhs_info> rhs
%type <Loc.t Ast.expr_info> expr

%%

let prog :=
  | p = list(chip); EOF;
    { p }

let chip :=
  | CHIP; name = PID; LBRACE;
    body = body;
    RBRACE;
      { let loc = Loc.make ($startpos) ($endpos) in
        mk_chip_info { body with Ast.name = name} loc
      }

let body :=
  | in_decl = in_decl;
    out_decl = out_decl;
    st_decl = option(st_decl);
    assign = list(assign);
      { { name=""
        ; input=in_decl
        ; output=out_decl
        ; states=Option.value ~default:StrMap.empty st_decl
        ; parts=assign
      } }

let in_decl :=
  | INPUT; id = LID; COLON; t = typ;
    { { name=id; typ=t } }

let out_decl :=
  | OUTPUT; id = LID; COLON; t = typ;
    { { name=id; typ=t } }

let st_decl :=
  | STATES; st_list=separated_nonempty_list(COMMA, state);
    { List.fold_left
        add_to_map
        StrMap.empty
        st_list
    }

let state :=
  | id = LID; COLON; t = typ;
    { let loc = Loc.make ($startpos(id)) ($endpos(id)) in
      Loc.wrap ~loc:loc (id, t)
    }

let typ :=
  | BITS; n = INT;
    { TBits (int_of_string n) }
  | LBRACE; body = typ_rbody; RBRACE;
    { TRecord body }

let typ_rbody :=
  | flist = separated_nonempty_list(COMMA, typ_rfield);
    { List.fold_left
        add_to_map
        StrMap.empty
        flist
    }

let typ_rfield :=
  | field = LID; COLON; t = typ;
    { let loc = Loc.make ($startpos(field)) ($endpos(field)) in
      Loc.wrap ~loc:loc (field, t)
    }

let assign :=
  | id = LID; COMB_DEF; rhs = rhs;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_assign_info (Current (id, rhs)) loc
    }
  | id = LID; PRIME; SEQ_DEF; rhs = rhs;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_assign_info (Next (id, rhs)) loc
    }

let rhs :=
  | expr = expr;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_rhs_info (Expr expr) loc
    }
  | chip = PID; expr = expr;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_rhs_info (Call (chip, expr)) loc
    }

let expr :=
  | bv = INT;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Const (of_binary bv)) loc
    }

  | LBRACE; body = expr_rbody; RBRACE;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Record body) loc
    }

  | expr = expr; LSQUARE; lb = INT; COLON; ub = INT; RSQUARE;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Splice (expr, int_of_string lb, int_of_string ub)) loc
    }

  | expr = expr; LSQUARE; idx = INT; RSQUARE;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Splice (expr, int_of_string idx, int_of_string idx)) loc
    }

  | expr1 = expr; PLUSPLUS; expr2 = expr;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Concat (expr1, expr2)) loc
    }

  | id = LID;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Access id) loc
    }

  | expr = expr; DOT; field = LID;
    { let loc = Loc.make ($startpos) ($endpos) in
      mk_expr_info (Member (expr, field)) loc
    }

  | LPAREN; expr = expr; RPAREN;
    { expr }

// let call :=
//   | chip = PID; expr = expr;
//     { let loc = Loc.make ($startpos) ($endpos) in
//       mk_call_info (CallE (chip, expr)) loc
//     }
//   | chip = PID; args = args;
//     { let loc = Loc.make ($startpos) ($endpos) in
//       mk_call_info (CallR (chip, args)) loc
//     }

// let args :=
//   | LBRACE; body = arg_rbody; RBRACE;
//     { body }

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
