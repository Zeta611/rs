let get_program stdin = Parser.program Lexer.token (Lexing.from_channel stdin)
