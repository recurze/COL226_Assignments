type token =
  | INT of (int)
  | TIMES
  | DIV
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
