{
open Parser        (* The type token is defined in parser.mli *)
open Typefile
exception Eof
}

let integer = ('0'|['1'-'9']['0'-'9']*)
let abs = "abs"
let or_ = "\\/"
let and_ = "/\\"
let equal = '='
let true_ = 'T'
let false_ = 'F'
let not_ = "not"
let op = '('
let cp = ')'
let gt = '>'
let lt = '<'
let gte = ">="
let lte = "<="
let add = '+'
let sub = '-'
let minus = '~'
let mul = '*'
let div = "div"
let exp = '^'
let mod_ = "mod"
let whitespace = [' ''\t']
let eol = '\n'
let var = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']*

rule translate = parse
| integer as numb { INT(int_of_string numb) }
| minus { MINUS }
| abs { ABS }
| add { ADD }
| sub { SUB }
| mul { MUL }
| div { DIV }
| mod_ { MOD }
| op { LPAREN }
| cp { RPAREN }
| true_ { TRUE }
| false_ { FALSE }
| not_ { NOT }
| or_ { OR }
| and_ { AND }
| equal { EQUAL }
| gt { GRT }
| lt { LESS }
| lte { LEQ } 
| gte { GEQ }
| var as variable { VAR(variable) }
| whitespace {translate lexbuf}
| eol { EOL }
| [^' ''~''\t''\n''+''-''=''*''('')''>''<''/''\\''T''F']+ as c  { (failwith (Printf.sprintf "Invalid Token \"%s\"\n" c)) }
| ['/''\\'] as c  { ( failwith (Printf.sprintf "Invalid Character \"%c\"\n" c) ) }
| eof   { raise Eof }

