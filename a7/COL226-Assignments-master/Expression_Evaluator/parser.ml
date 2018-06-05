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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
   open Typefile
# 30 "parser.ml"
let yytransl_const = [|
  258 (* TRUE *);
  259 (* FALSE *);
  261 (* ABS *);
  262 (* NOT *);
  263 (* MINUS *);
  264 (* OR *);
  265 (* AND *);
  266 (* ADD *);
  267 (* SUB *);
  268 (* MUL *);
  269 (* DIV *);
  270 (* MOD *);
  271 (* LESS *);
  272 (* GRT *);
  273 (* LEQ *);
  274 (* GEQ *);
  275 (* EQUAL *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  260 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\005\000\
\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\001\000\003\000\003\000\003\000\003\000\001\000\
\002\000\002\000\002\000\003\000\002\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\022\000\023\000\024\000\025\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\011\000\016\000\
\017\000\018\000\019\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\020\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\012\000\014\000\015\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000"

let yysindex = "\009\000\
\002\255\000\000\000\000\000\000\000\000\000\000\002\255\002\255\
\002\255\010\255\002\255\000\000\155\255\179\255\000\000\000\000\
\000\000\000\000\000\000\000\000\168\255\002\255\002\255\002\255\
\002\255\002\255\002\255\002\255\002\255\000\000\002\255\002\255\
\002\255\002\255\000\000\179\255\179\255\179\255\179\255\179\255\
\179\255\179\255\179\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\255\050\255\065\255\080\255\095\255\
\110\255\125\255\140\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\001\000\248\255\249\255\000\000"

let yytablesize = 193
let yytable = "\017\000\
\018\000\019\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\001\000\020\000\021\000\010\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\011\000\000\000\044\000\
\045\000\046\000\047\000\002\000\000\000\002\000\002\000\000\000\
\000\000\000\000\002\000\002\000\002\000\002\000\002\000\000\000\
\002\000\002\000\005\000\000\000\005\000\005\000\000\000\000\000\
\000\000\005\000\005\000\005\000\005\000\005\000\000\000\005\000\
\005\000\003\000\000\000\003\000\003\000\000\000\000\000\000\000\
\003\000\003\000\003\000\003\000\003\000\000\000\003\000\003\000\
\004\000\000\000\004\000\004\000\000\000\000\000\000\000\004\000\
\004\000\004\000\004\000\004\000\000\000\004\000\004\000\006\000\
\000\000\006\000\006\000\000\000\000\000\000\000\006\000\006\000\
\006\000\006\000\006\000\000\000\006\000\006\000\007\000\000\000\
\007\000\007\000\000\000\000\000\000\000\007\000\007\000\007\000\
\007\000\007\000\000\000\007\000\007\000\008\000\000\000\008\000\
\008\000\000\000\000\000\000\000\008\000\008\000\008\000\008\000\
\008\000\000\000\008\000\008\000\009\000\000\000\009\000\009\000\
\000\000\000\000\000\000\009\000\009\000\009\000\009\000\009\000\
\000\000\009\000\009\000\010\000\000\000\010\000\010\000\000\000\
\000\000\000\000\010\000\010\000\010\000\010\000\010\000\000\000\
\010\000\010\000\022\000\000\000\023\000\024\000\000\000\000\000\
\000\000\025\000\026\000\027\000\028\000\029\000\000\000\022\000\
\030\000\023\000\024\000\000\000\000\000\000\000\025\000\026\000\
\027\000\028\000\029\000\031\000\035\000\000\000\032\000\033\000\
\034\000"

let yycheck = "\007\000\
\008\000\009\000\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\001\000\001\001\011\000\011\001\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\020\001\255\255\031\000\
\032\000\033\000\034\000\008\001\255\255\010\001\011\001\255\255\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\255\255\
\021\001\022\001\008\001\255\255\010\001\011\001\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\255\255\021\001\
\022\001\008\001\255\255\010\001\011\001\255\255\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\255\255\021\001\022\001\
\008\001\255\255\010\001\011\001\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\255\255\021\001\022\001\008\001\
\255\255\010\001\011\001\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\255\255\021\001\022\001\008\001\255\255\
\010\001\011\001\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\255\255\021\001\022\001\008\001\255\255\010\001\
\011\001\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\019\001\255\255\021\001\022\001\008\001\255\255\010\001\011\001\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\255\255\021\001\022\001\008\001\255\255\010\001\011\001\255\255\
\255\255\255\255\015\001\016\001\017\001\018\001\019\001\255\255\
\021\001\022\001\008\001\255\255\010\001\011\001\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\255\255\008\001\
\022\001\010\001\011\001\255\255\255\255\255\255\015\001\016\001\
\017\001\018\001\019\001\009\001\021\001\255\255\012\001\013\001\
\014\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  ABS\000\
  NOT\000\
  MINUS\000\
  OR\000\
  AND\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  LESS\000\
  GRT\000\
  LEQ\000\
  GEQ\000\
  EQUAL\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                              ( _1 )
# 191 "parser.ml"
               : Typefile.parse_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 40 "parser.mly"
                              ( Elepar("expr",_1) )
# 198 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 41 "parser.mly"
                              ( Bipar("expr",Add,_1,_3) )
# 206 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 42 "parser.mly"
                              ( Bipar("expr",Sub,_1,_3) )
# 214 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 43 "parser.mly"
                              ( Bipar("expr",Or,_1,_3) )
# 222 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 44 "parser.mly"
                              ( Bipar("expr",Less,_1,_3) )
# 230 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 45 "parser.mly"
                              ( Bipar("expr",Grt,_1,_3) )
# 238 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 46 "parser.mly"
                              ( Bipar("expr",Leq,_1,_3) )
# 246 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 47 "parser.mly"
                              ( Bipar("expr",Geq,_1,_3) )
# 254 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 48 "parser.mly"
                              ( Bipar("expr",Equal,_1,_3) )
# 262 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 52 "parser.mly"
                              ( Elepar("subexp",_1) )
# 269 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 53 "parser.mly"
                              ( Bipar("subexp",Mul,_1,_3) )
# 277 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 54 "parser.mly"
                              ( Bipar("subexp",And,_1,_3) )
# 285 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 55 "parser.mly"
                              ( Bipar("subexp",Div, _1,_3) )
# 293 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'subexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 56 "parser.mly"
                              ( Bipar("subexp",Mod,_1,_3) )
# 301 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'base) in
    Obj.repr(
# 60 "parser.mly"
                              ( Elepar("unary",_1) )
# 308 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 61 "parser.mly"
                              ( Unipar("unary",Abs,_2) )
# 315 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 62 "parser.mly"
                              ( Unipar("unary",Not,_2) )
# 322 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 63 "parser.mly"
                              ( Unipar("unary",Minus,_2) )
# 329 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                              ( Elepar("base",_2) )
# 336 "parser.ml"
               : 'base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "parser.mly"
                              ( Elepar("base",Base(Int(-1 * _2))) )
# 343 "parser.ml"
               : 'base))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser.mly"
                              ( Elepar("base",Base(Int(_1))) )
# 350 "parser.ml"
               : 'base))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                              ( Elepar("base",Base(Bool(true))) )
# 356 "parser.ml"
               : 'base))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                              ( Elepar("base",Base(Bool(false))) )
# 362 "parser.ml"
               : 'base))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                              ( Elepar("base",Base(Var(_1))) )
# 369 "parser.ml"
               : 'base))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Typefile.parse_tree)
