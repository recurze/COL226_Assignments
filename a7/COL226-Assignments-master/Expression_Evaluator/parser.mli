type token =
  | INT of (int)
  | TRUE
  | FALSE
  | VAR of (string)
  | ABS
  | NOT
  | MINUS
  | OR
  | AND
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LESS
  | GRT
  | LEQ
  | GEQ
  | EQUAL
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Typefile.parse_tree
