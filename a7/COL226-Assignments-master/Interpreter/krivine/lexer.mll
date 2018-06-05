{
  open Parser
  exception Eof
}

let sep = ";;"
let equal = "="
let not_equal = ("\\=" | "\\==")
let not = ("not")
let var = (['a'-'z''A'-'Z''_']['0'-'9''a'-'z''A'-'Z''_']*)
let int_ = ('0'|['1'-'9']['0'-'9']*)
let true_ = "true"
let false_ = "false"
let let_ = "let"
let if_ = "if"
let func_ = "func"
let then_ = "then"
let else_ = "else"
let o_paren = '('
let c_paren = ')'
let or_ = "||"
let and_ = "&&"
let mul_ = "*"
let add_ = "+"
let sub_ = "-"
let less_ = "<"
let grt_ = ">"
let less_eq_ = "<="
let grt_eq_ = ">="
let arrow = "->"
let whitespace = [' ''\t']+
let eol = '\n'
let comment_line = "%"
let comment_block = "(*"
let comment_block_end = "*)"

rule translate = parse
| true_             {(*let () = Printf.printf "TRUE\n" in *) BOOLEAN_(true)}
| false_            {(*let () = Printf.printf "FALSE\n" in *) BOOLEAN_(false)}
| sep               {(*let () = Printf.printf "SEP\n" in *) SEP_}
| not               {(*let () = Printf.printf "NOT\n" in *) NOT_ }
| func_             {(*let () = Printf.printf "FUNC Start\n" in *) FUNC_}
| let_              {(*let () = Printf.printf "LET\n" in *) LET_ }
| if_               {(*let () = Printf.printf "IF\n" in *) IF_ }
| then_             {(*let () = Printf.printf "THEN\n" in *) THEN_ }
| else_             {(*let () = Printf.printf "ELSE\n" in *) ELSE_ }
| arrow             {(*let () = Printf.printf "ARROW\n" in *) ARROW_ }
| comment_line      {(*let () = Printf.printf "COM START\n" in *) single_line lexbuf}
| comment_block     {(*let () = Printf.printf "COM BLOCK START\n" in *) multi_line lexbuf}
| var as variable   {(*let () = Printf.printf "VAR(variable)\n" in *) VAR_(variable)}
| int_ as integer   {(*let () = Printf.printf "INTEGER\n" in *) INTEGER_(int_of_string integer)}
| equal             {(*let () = Printf.printf "EQUAL\n" in *) EQUAL_}
| not_equal         {(*let () = Printf.printf "NOT_EQ\n" in *) NOT_EQUAL_}
| o_paren           {(*let () = Printf.printf "O_PAREN\n" in *) O_PAREN_}
| c_paren           {(*let () = Printf.printf "C_PAREN\n" in *) C_PAREN_}
| or_               {(*let () = Printf.printf "OR_\n" in *) OR_}
| and_              {(*let () = Printf.printf "AND_\n" in *) AND_}
| mul_              {(*let () = Printf.printf "MUL_\n" in *) MUL_}
| add_              {(*let () = Printf.printf "ADD_\n" in *) ADD_}
| sub_              {(*let () = Printf.printf "SUB_\n" in *) SUB_}
| less_             {(*let () = Printf.printf "LESS_\n" in *) LESS_ }
| grt_              {(*let () = Printf.printf "LESS_\n" in *) GRT_ }
| less_eq_          {(*let () = Printf.printf "LESS_\n" in *) LESS_EQ_ }
| grt_eq_           {(*let () = Printf.printf "LESS_\n" in *) GRT_EQ_ }
| eol               {(*let () = Printf.printf "NEWLINE \n" in *) translate lexbuf }
| whitespace        {(*let () = Printf.printf "Whitespace \n" in *) translate lexbuf }
| eof               {(*let () = Printf.printf "EOF \n" in *) EOF }

and single_line = parse
| eof { translate lexbuf }
| eol { translate lexbuf }
| _   { single_line lexbuf }

and multi_line = parse
| comment_block_end {translate lexbuf}
| eof {raise Eof}
| _ {multi_line lexbuf}
