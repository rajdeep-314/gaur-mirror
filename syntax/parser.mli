val lex_string : string -> Lexer.token list
val parse_prog : string -> Ast.prog
val parse_from_path : string -> Ast.prog

