type variables = Var of string;;
type symbol = S of string;;
type signature = (symbol * int) list;;

type term = V of variables | Node of symbol*(term list);;
type sigma = (variables * term) list;;
