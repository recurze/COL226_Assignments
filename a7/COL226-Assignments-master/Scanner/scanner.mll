{ }
let integer = '-'?('0'|['1'-'9']['0'-'9']*)
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
let if_ = "if"
let then_ = "then"
let else_ = "else"
let id = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*
let func = "def"
let term = ';'
let add = '+'
let sub = '-'
let mul = '*'
let div = "div"
let exp = '^'
let mod_ = "mod"
let whitespace = [' ''\t''\n']

rule translate = parse
| integer as numb { Printf.printf "Integer(%s)\n" numb; translate lexbuf }
| abs { Printf.printf "Unary_Operator(abs)\n"; translate lexbuf }
| add { Printf.printf "Binary_Operator(+)\n"; translate lexbuf }
| sub { Printf.printf "Binary_Operator(-)\n"; translate lexbuf }
| mul { Printf.printf "Binary_Operator(*)\n"; translate lexbuf }
| div { Printf.printf "Binary_Operator(div)\n"; translate lexbuf }
| mod_ { Printf.printf "Binary_Operator(mod)\n"; translate lexbuf }
| exp { Printf.printf "Binary_Operator(exp)\n"; translate lexbuf }
| op { Printf.printf "Parenthesis(open)\n"; translate lexbuf }
| cp { Printf.printf "Parenthesis(close)\n"; translate lexbuf }
| true_ { Printf.printf "Boolean_Const(T)\n"; translate lexbuf }
| false_ { Printf.printf "Boolean_Const(F)\n"; translate lexbuf }
| not_ { Printf.printf "Unary_Bool_Op(not)\n"; translate lexbuf }
| or_ { Printf.printf "Binary_Bool_Op(or)\n"; translate lexbuf }
| and_ { Printf.printf "Binary_Bool_Op(and)\n"; translate lexbuf }
| equal { Printf.printf "Comparison_Op(=)\n"; translate lexbuf }
| gt { Printf.printf "Comparison_Op(>)\n"; translate lexbuf }
| lt { Printf.printf "Comparison_Op(<)\n"; translate lexbuf }
| lte { Printf.printf "Comparison_Op(<=)\n"; translate lexbuf } 
| gte { Printf.printf "Comparison_Op(>=)\n"; translate lexbuf }
| if_ {Printf.printf "Conditional_Op(if)\n"; translate lexbuf }
| then_ {Printf.printf "Conditional_Op(then)\n"; translate lexbuf }
| else_ {Printf.printf "Conditional_Op(else)\n"; translate lexbuf }
| func {Printf.printf "Definition_Construct\n"; translate lexbuf }
| id as text {Printf.printf "Identifier(%s)\n" text; translate lexbuf }
| term {Printf.printf "Delimiter\n"; translate lexbuf }
| whitespace {translate lexbuf}
| [^' ''\t''\n''+''-''=''*'';''^''('')''>''<''/''\\''T''F']+ as c  { Printf.printf "Invalid Token \"%s\"\n" c; translate lexbuf }
| ['/''\\'] as c  { Printf.printf "Invalid Character \"%c\"\n" c; translate lexbuf }
| eof   { exit 0 }

{
	let main () =
	let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in 
	let lexbuf = Lexing.from_channel cin in
	translate lexbuf
	let _ = Printexc.print main ()
}