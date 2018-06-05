# 1 "lexer.mll"
 
open Parser        (* The type token is defined in parser.mli *)
open Typefile
exception Eof

# 8 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\230\255\089\000\233\255\234\255\192\000\002\000\003\000\
    \240\255\001\000\011\000\050\001\129\001\213\001\246\255\247\255\
    \071\002\083\002\250\255\251\255\252\255\129\000\254\255\151\002\
    \205\002\231\002\037\003\063\003\117\003\174\003\220\003\026\004\
    \140\004\152\004\242\255\241\255\236\255\237\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\023\000\255\255\255\255\020\000\017\000\016\000\
    \255\255\024\000\024\000\023\000\011\000\010\000\255\255\255\255\
    \023\000\023\000\255\255\255\255\255\255\023\000\255\255\000\000\
    \000\000\023\000\002\000\023\000\006\000\023\000\007\000\020\000\
    \023\000\012\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\002\000\000\000\000\000\002\000\255\255\255\255\
    \000\000\255\255\255\255\002\000\255\255\255\255\000\000\000\000\
    \002\000\002\000\000\000\000\000\000\000\002\000\000\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\255\255\
    \002\000\002\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\004\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \015\000\014\000\018\000\020\000\000\000\019\000\000\000\009\000\
    \024\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\034\000\000\000\006\000\008\000\007\000\037\000\
    \036\000\005\000\005\000\005\000\005\000\005\000\012\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\013\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\010\000\035\000\000\000\000\000\
    \000\000\021\000\255\255\255\255\017\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\016\000\011\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\022\000\000\000\
    \000\000\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \255\255\000\000\000\000\025\000\000\000\000\000\000\000\005\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\000\000\255\255\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \001\000\005\000\005\000\005\000\005\000\005\000\031\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\031\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\255\255\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\255\255\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
    \031\000\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \000\000\000\000\255\255\000\000\031\000\000\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\000\000\000\000\000\000\
    \255\255\255\255\000\000\255\255\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\029\000\255\255\
    \000\000\000\000\000\000\000\000\027\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \255\255\255\255\000\000\255\255\000\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\026\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\255\255\000\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\255\255\000\000\028\000\000\000\255\255\
    \255\255\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\255\255\255\255\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\030\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\000\000\000\000\
    \000\000\031\000\000\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\255\255\000\000\000\000\000\000\006\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\009\000\255\255\255\255\
    \255\255\000\000\002\000\002\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\002\000\002\000\002\000\002\000\255\255\002\000\255\255\
    \002\000\255\255\021\000\021\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\002\000\002\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\000\
    \255\255\021\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\021\000\021\000\021\000\021\000\002\000\021\000\255\255\
    \021\000\255\255\255\255\255\255\255\255\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\021\000\021\000\021\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\021\000\
    \255\255\005\000\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\021\000\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\021\000\255\255\255\255\
    \005\000\255\255\255\255\021\000\255\255\255\255\255\255\005\000\
    \005\000\005\000\005\000\005\000\255\255\005\000\255\255\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\255\255\255\255\005\000\005\000\005\000\021\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\255\255\005\000\255\255\255\255\005\000\
    \255\255\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\011\000\011\000\255\255\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\011\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\002\000\011\000\011\000\011\000\011\000\255\255\011\000\
    \255\255\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\011\000\011\000\
    \011\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \011\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\021\000\255\255\255\255\255\255\255\255\011\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\011\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \012\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\255\255\255\255\255\255\255\255\255\255\
    \005\000\255\255\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\255\255\255\255\255\255\255\255\
    \012\000\255\255\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\013\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \255\255\255\255\011\000\255\255\013\000\255\255\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \016\000\016\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\017\000\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\016\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\016\000\
    \016\000\016\000\016\000\017\000\016\000\255\255\016\000\255\255\
    \255\255\255\255\255\255\017\000\017\000\017\000\017\000\255\255\
    \017\000\255\255\017\000\016\000\016\000\016\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\255\255\017\000\
    \017\000\017\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\017\000\255\255\016\000\255\255\255\255\255\255\255\255\
    \023\000\023\000\255\255\016\000\255\255\255\255\255\255\017\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\017\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\016\000\023\000\
    \255\255\255\255\255\255\255\255\017\000\255\255\255\255\023\000\
    \023\000\023\000\023\000\255\255\023\000\016\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\017\000\255\255\023\000\023\000\023\000\024\000\024\000\
    \255\255\255\255\255\255\255\255\255\255\023\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\023\000\255\255\024\000\255\255\255\255\
    \025\000\025\000\255\255\023\000\255\255\024\000\024\000\024\000\
    \024\000\255\255\024\000\255\255\024\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\025\000\
    \255\255\024\000\024\000\024\000\255\255\255\255\255\255\025\000\
    \025\000\025\000\025\000\024\000\025\000\023\000\025\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\024\000\255\255\025\000\025\000\025\000\255\255\255\255\
    \255\255\024\000\255\255\255\255\255\255\025\000\026\000\026\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\025\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\025\000\255\255\026\000\255\255\016\000\
    \027\000\027\000\255\255\024\000\255\255\026\000\026\000\026\000\
    \026\000\255\255\026\000\017\000\026\000\255\255\255\255\255\255\
    \255\255\255\255\025\000\255\255\255\255\255\255\255\255\027\000\
    \255\255\026\000\026\000\026\000\255\255\025\000\255\255\027\000\
    \027\000\027\000\027\000\026\000\027\000\255\255\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\026\000\255\255\027\000\027\000\027\000\028\000\028\000\
    \255\255\026\000\255\255\255\255\255\255\027\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\000\255\255\028\000\255\255\023\000\
    \255\255\255\255\255\255\027\000\255\255\028\000\028\000\028\000\
    \028\000\255\255\028\000\026\000\028\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\028\000\028\000\028\000\255\255\027\000\255\255\029\000\
    \029\000\255\255\255\255\028\000\255\255\027\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\028\000\255\255\255\255\255\255\024\000\029\000\255\255\
    \255\255\028\000\255\255\255\255\255\255\255\255\029\000\029\000\
    \029\000\029\000\255\255\029\000\255\255\029\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\030\000\030\000\025\000\
    \255\255\255\255\029\000\029\000\029\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\000\029\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\030\000\255\255\255\255\255\255\
    \255\255\255\255\029\000\255\255\030\000\030\000\030\000\030\000\
    \255\255\030\000\029\000\030\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\029\000\255\255\255\255\255\255\255\255\255\255\
    \030\000\030\000\030\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\030\000\255\255\255\255\026\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\029\000\255\255\255\255\255\255\
    \030\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \030\000\255\255\255\255\255\255\255\255\255\255\255\255\027\000\
    \255\255\031\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\030\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\028\000\255\255\255\255\
    \255\255\031\000\255\255\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\032\000\032\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\033\000\033\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\032\000\255\255\029\000\255\255\
    \255\255\255\255\255\255\255\255\032\000\032\000\032\000\032\000\
    \033\000\032\000\255\255\032\000\255\255\255\255\255\255\255\255\
    \033\000\033\000\033\000\033\000\255\255\033\000\255\255\033\000\
    \032\000\032\000\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\032\000\255\255\033\000\033\000\033\000\255\255\
    \255\255\255\255\255\255\255\255\030\000\255\255\033\000\255\255\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \032\000\255\255\255\255\255\255\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\033\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \033\000";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec translate lexbuf =
    __ocaml_lex_translate_rec lexbuf 0
and __ocaml_lex_translate_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 33 "lexer.mll"
             numb
# 412 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 33 "lexer.mll"
                  ( INT(int_of_string numb) )
# 416 "lexer.ml"

  | 1 ->
# 34 "lexer.mll"
        ( MINUS )
# 421 "lexer.ml"

  | 2 ->
# 35 "lexer.mll"
      ( ABS )
# 426 "lexer.ml"

  | 3 ->
# 36 "lexer.mll"
      ( ADD )
# 431 "lexer.ml"

  | 4 ->
# 37 "lexer.mll"
      ( SUB )
# 436 "lexer.ml"

  | 5 ->
# 38 "lexer.mll"
      ( MUL )
# 441 "lexer.ml"

  | 6 ->
# 39 "lexer.mll"
      ( DIV )
# 446 "lexer.ml"

  | 7 ->
# 40 "lexer.mll"
       ( MOD )
# 451 "lexer.ml"

  | 8 ->
# 41 "lexer.mll"
     ( LPAREN )
# 456 "lexer.ml"

  | 9 ->
# 42 "lexer.mll"
     ( RPAREN )
# 461 "lexer.ml"

  | 10 ->
# 43 "lexer.mll"
        ( TRUE )
# 466 "lexer.ml"

  | 11 ->
# 44 "lexer.mll"
         ( FALSE )
# 471 "lexer.ml"

  | 12 ->
# 45 "lexer.mll"
       ( NOT )
# 476 "lexer.ml"

  | 13 ->
# 46 "lexer.mll"
      ( OR )
# 481 "lexer.ml"

  | 14 ->
# 47 "lexer.mll"
       ( AND )
# 486 "lexer.ml"

  | 15 ->
# 48 "lexer.mll"
        ( EQUAL )
# 491 "lexer.ml"

  | 16 ->
# 49 "lexer.mll"
     ( GRT )
# 496 "lexer.ml"

  | 17 ->
# 50 "lexer.mll"
     ( LESS )
# 501 "lexer.ml"

  | 18 ->
# 51 "lexer.mll"
      ( LEQ )
# 506 "lexer.ml"

  | 19 ->
# 52 "lexer.mll"
      ( GEQ )
# 511 "lexer.ml"

  | 20 ->
let
# 53 "lexer.mll"
         variable
# 517 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 53 "lexer.mll"
                  ( VAR(variable) )
# 521 "lexer.ml"

  | 21 ->
# 54 "lexer.mll"
             (translate lexbuf)
# 526 "lexer.ml"

  | 22 ->
# 55 "lexer.mll"
      ( EOL )
# 531 "lexer.ml"

  | 23 ->
let
# 56 "lexer.mll"
                                                             c
# 537 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 56 "lexer.mll"
                                                                ( (failwith (Printf.sprintf "Invalid Token \"%s\"\n" c)) )
# 541 "lexer.ml"

  | 24 ->
let
# 57 "lexer.mll"
               c
# 547 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 57 "lexer.mll"
                  ( ( failwith (Printf.sprintf "Invalid Character \"%c\"\n" c) ) )
# 551 "lexer.ml"

  | 25 ->
# 58 "lexer.mll"
        ( raise Eof )
# 556 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_translate_rec lexbuf __ocaml_lex_state

;;
