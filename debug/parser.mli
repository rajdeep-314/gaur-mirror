val lex_string : string -> Lexer.token list

val parse_line_from_toks_opt : Lexer.token list -> Ast.t option
val parse_line_from_toks : Lexer.token list -> Ast.t

val parse_line_opt : string -> Ast.t option
val parse_line : string -> Ast.t

