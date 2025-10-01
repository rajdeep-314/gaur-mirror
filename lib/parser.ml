open Lexer
open Ast.Helper

module Reg = Ast.Reg

let lex_string s =
    let lexbuf = Lexing.from_string s in
    let rec aux () =
        match Lexer.token lexbuf with
        | EOF -> []
        | tok -> tok :: aux ()
    in
    aux ()


let parse_inst_from_toks = function
    | LLI :: REG rd :: IMM imm :: ts ->
            (lli (Reg.of_int rd) imm, ts)
    | LUI :: REG rd :: IMM imm :: ts ->
            (lui (Reg.of_int rd) imm, ts)
    | LD :: REG rd :: REG rs :: IMM imm :: ts ->
            (ld (Reg.of_int rd) (Reg.of_int rs) imm, ts)
    | ST :: REG rs1 :: REG rs2 :: IMM imm :: ts ->
            (st (Reg.of_int rs1) (Reg.of_int rs2) imm, ts)
    | ADD :: REG rd :: REG rs1 :: REG rs2 :: ts ->
            (add (Reg.of_int rd) (Reg.of_int rs1) (Reg.of_int rs2), ts)
    | ADDI :: REG rd :: REG rs :: IMM imm :: ts ->
            (addi (Reg.of_int rd) (Reg.of_int rs) imm, ts)
    | SUB :: REG rd :: REG rs1 :: REG rs2 :: ts ->
            (sub (Reg.of_int rd) (Reg.of_int rs1) (Reg.of_int rs2), ts)
    | AND :: REG rd :: REG rs1 :: REG rs2 :: ts ->
            (and_ (Reg.of_int rd) (Reg.of_int rs1) (Reg.of_int rs2), ts)
    | NOT :: REG rd :: REG rs :: ts ->
            (not (Reg.of_int rd) (Reg.of_int rs), ts)
    | BEQ :: REG rs1 :: REG rs2 :: IMM imm :: ts ->
            (beq (Reg.of_int rs1) (Reg.of_int rs2) imm, ts)
    | BLT :: REG rs1 :: REG rs2 :: IMM imm :: ts ->
            (blt (Reg.of_int rs1) (Reg.of_int rs2) imm, ts)
    | JALR :: REG rd :: REG rs :: IMM imm :: ts ->
            (jalr (Reg.of_int rd) (Reg.of_int rs) imm, ts)
    | APC :: REG rd :: IMM imm :: ts ->
            (apc (Reg.of_int rd) imm, ts)
    | J :: IMM imm :: ts ->
            (j imm, ts)
    | _ ->
            failwith "Parsing error: Invalid syntax."


let rec parse_prog_from_toks toks =
    match toks with
    | [] -> []
    | [EOF] -> []
    | NEWLINE :: ts -> parse_prog_from_toks ts
    | _ ->
            let (inst, ts) = parse_inst_from_toks toks in
            inst :: parse_prog_from_toks ts


let parse_prog s =
    parse_prog_from_toks (lex_string s)


let parse_file ic =
    let content = really_input_string ic (in_channel_length ic) in
    parse_prog content


let parse_from_path path =
    parse_file (open_in path)
    
