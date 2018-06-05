type variable = Var of string;;
type opcode = INTEGER of int
              | BOOL of bool 
              | ADD | SUB | OR | AND | EQUAL | MUL
              | LESS | GRT | LESS_EQ | GRT_EQ
              | LET of variable | ENDLET 
              | APPLY | RETURN 
              | CLOSURE of (opcode list) 
              | ACCESS of variable 
              | IF_THEN_ELSE of ((opcode list) * (opcode list))
;;
type closure = VCL_INT of int| VCL_BOOL of bool | CL of environment * (opcode list) and
environment = (variable * closure) list;;
type stack = closure list;;
type dump = (environment * (opcode list)) list;;