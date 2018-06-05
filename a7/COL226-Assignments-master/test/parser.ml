type token =
  | INT of (int)
  | TIMES
  | DIV
  | EOL

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* TIMES *);
  259 (* DIV *);
  260 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\006\000\007\000\000\000\000\000\000\000\001\000\
\000\000\000\000\003\000\005\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\007\000"

let yysindex = "\002\000\
\000\255\000\000\000\000\000\000\001\255\002\255\003\255\000\000\
\000\255\000\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\004\255\254\254\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\255\255\000\000"

let yytablesize = 9
let yytable = "\004\000\
\003\000\004\000\001\000\009\000\008\000\010\000\011\000\002\000\
\012\000"

let yycheck = "\002\001\
\001\001\004\001\001\000\002\001\004\001\003\001\009\000\004\001\
\010\000"

let yynames_const = "\
  TIMES\000\
  DIV\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'subexp) in
    Obj.repr(
# 10 "parser.mly"
                              ( _1 )
# 70 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unit) in
    Obj.repr(
# 13 "parser.mly"
                             ( _1 )
# 77 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'unit) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'subexp) in
    Obj.repr(
# 14 "parser.mly"
                                ( _1 * _3 )
# 85 "parser.ml"
               : 'subexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'base) in
    Obj.repr(
# 16 "parser.mly"
                    (_1)
# 92 "parser.ml"
               : 'unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'base) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unit) in
    Obj.repr(
# 17 "parser.mly"
                    (_1 / _3)
# 100 "parser.ml"
               : 'unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "parser.mly"
          (_1)
# 107 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
