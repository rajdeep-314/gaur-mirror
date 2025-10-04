val lex_string : string -> Lexer.token list
val parse_line_from_toks : Lexer.token list -> Ast.t
val parse_line : string -> Ast.t

