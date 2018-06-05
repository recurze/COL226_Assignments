type token =
  | END
  | SEP
  | VAR of (string)
  | CONS of (string)
  | COMMA
  | O_PAREN
  | C_PAREN
  | O_SQ
  | C_SQ
  | OR
  | EOL
  | WHITESPACE
  | EOF
  | EQUAL
  | NOT_EQUAL
  | NOT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Structure.program
val goal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Structure.goal
