type exp = True |  False
        |  Const of int
        |  Abs   of exp
        |  Var of string
        |  Not   of exp
        |  List  of exp list 
        |  Plus  of exp * exp
        |  Minus of exp * exp
        |  Mul   of exp * exp
        |  Div   of exp * exp
        |  Mod   of exp * exp
        |  Pow   of exp * exp
        |  And   of exp * exp
        |  Orr   of exp * exp
        |  Imply of exp * exp
        |  Equal of exp * exp
        |  Great of exp * exp
        |  Lesss of exp * exp
        |  Grequ of exp * exp
        |  Leequ of exp * exp
        |  Projj of int * exp
        (* New additions *)
        |  LetInEnd of exp * exp * exp
        |  Lambda of exp * exp           (* \x.e1 *)
        |  Apply of exp * exp            (* e1 e2 *)
        |  Ite of exp * exp * exp        (* if then else *)
;;

type vclosure = environment * opcode * opcode list
and environment = (exp * answer) list
and answer =  Int of int 
            | Bool of bool 
            | Listans of answer list
            | VCL of vclosure
and opcode =   TRUE |  FALSE
            |  CONST of int
            |  VAR of string 
            |  LIST of opcode list list
            |  PLUS |  MINUS |  MUL |  DIV
            |  ABS |  MOD |  POW
            |  NOT |  AND |  ORR |  IMPLY
            |  EQUAL |  GREAT |  LESSS |  GREQU |  LEEQU
            (* New additions *)
            |  BIND of opcode | UNBIND of opcode
            |  APP |  RET
            |  CLOS of (opcode * (opcode list))
            |  COND of ((opcode list) * (opcode list))
;;


type closure = CLOSURE of table * exp
and table = (exp * closure) list 
;;