type variable = Var of string;;
type expression = BIND of (variable * expression)
                  | APPLY of (expression * expression) 
                  | FUNC of (variable  * expression)
                  
                  | MUL of (expression * expression)
                  | ADD of (expression * expression)
                  | SUB of (expression * expression)
                  | EQUAL of (expression * expression)
                  | OR of (expression * expression)
                  | AND of (expression * expression)
                  | LESS of (expression * expression)
                  | GRT of (expression * expression)
                  | LESS_EQ of (expression * expression)
                  | GRT_EQ of (expression * expression)

                  | IF_THEN_ELSE of (expression * expression * expression)

                  | VAR of variable
                  | INTEGER of int
                  | BOOL of bool
                  | NIL

;;
type closure = CL of (expression * environment ) and
environment = (variable * closure ) list
;;
type program = expression list;;
type stack = closure list;;
