type symbol = S of string * int;;

type term =  V of string
        |   Const of string
        |   Node of symbol * (term list)
        |   Cut
;;

type sigma = (string * term) list;;

type clauses = Fact of term | Rule of term * (term list);;
type program = clauses list;;
type goal = term list;;

