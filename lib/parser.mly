%{
  open Ast
  open Utils
  open Bits
  open Location
%}


%token <string> LID (* lowercase identifier *)
%token <string> PID (* PascalCase identifier *)
%token <int>    INT (* integer literal *)

%token BITS CHIP INPUT OUTPUT STATES

%token FIELD_ASSIGN
%token DOT
%token COLON
%token COMMA

%token LBRACE RBRACE
%token LPAREN RPAREN
%token LSQURE RSQURE

%token PLUSPLUS
%token PRIME
%token COMB_DEF
%token SEQ_DEF

%token EOF

%left  PLUSPLUS
%right  APP
%right  DOT
%left  LSQURE

%start <Ast.program> main

%%

main:
    | cs = chip* EOF
        { cs }

chip:
    | CHIP name=PID LBRACE body=chip_body RBRACE
        { { body with Ast.name=name } }

chip_body:
    | ind=in_decl outd=out_decl sts=states_decl? asgn=assign*
        { {
            name="";
            input=ind;
            output=outd;
            states=sts;
            parts=asgn;
        } }

in_decl:
    | INPUT id=LID COLON t=typ
        { {
            port_name=id;
            port_type=t
        } }

out_decl:
    | OUTPUT id=LID COLON t=typ
        { {
            port_name=id;
            port_type=t
        } }

states_decl:
    | STATES states_list=separated_nonempty_list(COMMA, state)
        {
            List.fold_left
                add_to_map
            (*  (fun acc (k, loc, v) -> add_to_map acc k loc v)  *)
                StrMap.empty
                states_list
        }

state:
    | id=LID COLON t=typ
        {
            let loc = mk_loc ($startpos(id)) ($endpos(id)) in
            wrap_loc ~loc:loc (id, t)
        }

typ:
    | BITS n=INT
        { TBits n }
    
    | LBRACE body=typ_record_body RBRACE
        { TRecord body }

typ_record_body:
    | typ_record_field_list = separated_nonempty_list(COMMA, typ_record_field)
        {
            List.fold_left
                add_to_map
            (*  (fun acc (k, loc, v) -> add_to_map acc k loc v)  *)
                StrMap.empty
                typ_record_field_list
        }

typ_record_field:
    | field=LID COLON t=typ
        {
            let loc = mk_loc ($startpos(field)) ($endpos(field)) in
            wrap_loc ~loc:loc (field, t)
        }

expression:
    | i = INT
        (* 0b010, 0xfd, 0o71, 32 *)
        {
            (* TODO: What do i need this for ? *)
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Constant (int_to_bits i))
        }
    
    | LPAREN expr=expression RPAREN
        (* (expr) *)
        {
            (* Pass parenthesized expression as is *)
            expr
        }

    | expr=expression DOT field=LID
        (* foo.bar *)
        {
            (* TODO: Should this be location of the field ? *)
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Member (expr, field))
        }
    
    | expr1=expression PLUSPLUS expr2=expression
        (* foo1 ++ bar2 *)
        {
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Concat (expr1, expr2))
        }

    | expr=expression LSQURE start_index=INT COLON end_index=INT RSQURE
        (* foo.bar[0:2] *)
        {
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Splice (expr, start_index, end_index))
        }

    | expr=expression LSQURE index=INT RSQURE
        (* foo.bar[0] *)
        {
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Splice (expr, index, index))
        }
    
    | LBRACE body=exp_record_body RBRACE
        (* {f1=e1, f2=e2, ..., fn=en} *)
        {
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Record body)
        }
    
    | chip=PID expr=expression
        (* Chip expr *)
        {
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Instance (chip, expr))
        } %prec APP
    
    | id=LID
        (* foo *)
        {
            let loc = mk_loc ($startpos) ($endpos) in
            mk_expr ~loc:loc (Access id)
        }

exp_record_body:
    | expr_record_field_list = separated_nonempty_list(COMMA, exp_record_field)
        {
            List.fold_left
                add_to_map
            (*  (fun acc (k, loc, v) -> add_to_map acc k loc v)  *)
                StrMap.empty
                expr_record_field_list
        }

exp_record_field:
    | field=LID FIELD_ASSIGN expr=expression
        {
            let loc = mk_loc ($startpos(field)) ($endpos(field)) in
            wrap_loc ~loc:loc (field, expr)
        }

assign:
    | id=LID COMB_DEF expr=expression
        {
            let loc = mk_loc ($startpos) ($endpos) in
            let assign_desc = mk_assign_desc id expr Current in
            mk_assign ~loc:loc assign_desc
        }

    | id=LID PRIME SEQ_DEF expr=expression
        {
            let loc = mk_loc ($startpos) ($endpos) in
            let assign_desc = mk_assign_desc id expr Next in
            mk_assign ~loc:loc assign_desc
        }
