type token =
  | INT of (int)
  | BOOL of (bool)
  | STRING of (string)
  | VAR of (string)
  | PLUS
  | AND
  | EQ
  | CAT
  | LET
  | LETF
  | IN
  | IF
  | THEN_
  | ELSE
  | PAIR
  | FST
  | SND
  | UNIT
  | SEMICOLON
  | PRINT
  | COMMA
  | EQEQ
  | SUBS
  | MULT
  | DIV
  | LEQ
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
