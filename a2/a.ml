(*
 * Assumptions made:
 *  1.No type checking, i.e., user doesn't/won't enter
 *    incompatible types under any operations.
 *    Eg: Plus (Const 5, True); And (False, Const 4); Not (List e)
 *  2.Arguments given to any Constructor agrees
 *    with the signature of that operator.
 *    Eg: Plus (Const 5, Const 4, Const 3); Abs (Const 1, Const 2) is invalid.
 *  3.For this function, assumed own rho,
 *    can be changed when the table is given.
 *  4.Projection is the only operator which acts on Tuples/Lists
 *    Add, Sub and other operations are not possible.
 *)

(*
    To compile: ocamlc -w -1..61 a.ml -o a //also suppresses all warnings.
    To run: ./a
    The code outputs all integers whose testcases have failed.
    If output has x then testcase x has failed i.e., ax failed.
 *)
type exp = True
        |  False
        |  Const of int
        |  Var   of string
        |  Abs   of exp
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
        |  Projj of int * exp;;

type answer = Int of int | Bool of bool | Listans of answer list;;

let addd (a1,a2) = match (a1,a2) with
                    (Int aa1,Int aa2)->Int(aa1+aa2)
                |   _->Int 0;;

let subb (a1,a2) = match (a1,a2) with
                    (Int aa1,Int aa2)->Int(aa1-aa2)
                |   _->Int 0;;

let divv (a1,a2) = match (a1,a2) with
                    (Int aa1,Int aa2)->Int(aa1/aa2)
                |   _->Int 0;;

let mull (a1,a2) = match (a1,a2) with
                    (Int aa1,Int aa2)->Int(aa1*aa2)
                |   _->Int 0;;

let modd (a1,a2) = match (a1,a2) with
                    (Int aa1,Int aa2)->Int(aa1 mod aa2)
                |   _->Int 0;;

let rec expo (a1,a2) = match (a1,a2) with
                    (Int aa1,Int 0)->Int 1
                |   (Int aa1,Int aa2)->
                        let b = expo(a1,Int (aa2/2)) in
                        match b with Int x->
                            if aa2 mod 2 = 0 then Int (x*x)
                            else Int (x*x*aa1)
                |   _->Int 0;;

let abss a1 = match a1 with
                    Int aa1->if aa1<0 then Int (-aa1) else Int aa1
                |   _->Int 0;;

let nott a1 = match a1 with
                    Bool t->Bool (not t)
                |   _->Bool false;;

let andd (a1,a2) = match (a1,a2) with
                    (Bool aa1,Bool aa2)->Bool (aa1 && aa2)
                |   _->Bool false;;

let orrr (a1,a2) = match (a1,a2) with
                    (Bool aa1,Bool aa2)->Bool (aa1 || aa2)
                |   _->Bool false;;

let implyy (a1,a2) = match (a1,a2) with
                    (Bool aa1, Bool aa2)-> Bool (not aa1 || aa2)
                |   _->Bool false;;
let greatt (a1,a2) = match (a1,a2) with
                    (Int aa1, Int aa2)-> Bool (aa1>aa2)
                |   _->Bool false;;

let equall (a1,a2) = match (a1,a2) with
                    (Int aa1, Int aa2)->Bool (aa1=aa2)
                |   _->Bool false;;

let lessss (a1,a2) = match (a1,a2) with
                    (Int aa1, Int aa2)->Bool (aa1<aa2)
                |   _->Bool false;;

let grequu (a1,a2) = match (a1,a2) with
                    (Int aa1, Int aa2)->Bool (aa1>=aa2)
                |   _->Bool false;;

let leequu (a1,a2) = match (a1,a2) with
                    (Int aa1, Int aa2)->Bool (aa1<=aa2)
                |   _->Bool false;;

let rec mapp f a = match a with 
                    [x]->[f x]
                |   x::y->(f x)::(mapp f y);; 

let rho e = match e with "x"->Int 5 | "b" ->Bool true;;

let rec ith (i,l) = match l with 
                    x::y-> if i=0 then x else ith(i-1,y);;

let rec eval e = match e with
                True->              Bool true
              | False->             Bool false
              | Const n->           Int n
              | List e1->           Listans (mapp eval e1)
              | Var s->             rho s
              | Abs e1->            abss(eval e1)
              | Not e1->            nott(eval e1)
              | Plus (e1,e2)->      addd(eval e1, eval e2)
              | Minus (e1,e2)->     subb(eval e1, eval e2)
              | Mul (e1,e2)->       mull(eval e1, eval e2)
              | Div (e1,e2)->       divv(eval e1, eval e2)
              | Mod (e1,e2)->       modd(eval e1, eval e2)
              | Pow (e1,e2)->       expo(eval e1, eval e2)
              | And (e1,e2)->       andd(eval e1,eval e2)
              | Orr (e1,e2)->       orrr(eval e1,eval e2)
              | Imply (e1,e2)->     implyy(eval e1,eval e2)
              | Equal (e1,e2)->     equall(eval e1,eval e2)
              | Great (e1,e2)->     greatt(eval e1,eval e2)
              | Lesss (e1,e2)->     lessss(eval e1,eval e2)
              | Grequ (e1,e2)->     grequu(eval e1,eval e2)
              | Leequ (e1,e2)->     leequu(eval e1,eval e2)
              | Projj (i,List e1)-> eval (ith(i,e1));;

type opcode=
           TRUE
        |  FALSE
        |  CONST of int
        |  VAR of string 
        |  LIST of opcode list list
        |  ABS
        |  NOT
        |  PLUS
        |  MINUS
        |  MUL
        |  DIV
        |  MOD
        |  POW
        |  AND
        |  ORR
        |  IMPLY
        |  EQUAL
        |  GREAT
        |  LESSS
        |  GREQU
        |  LEEQU;;


let rec compile e = match e with
                True->              [TRUE]
              | False->             [FALSE]
              | Const n->           [CONST n]
              | Var s->             [VAR s]
              | List e->            [LIST (mapp compile e)]
              | Abs e1->            (compile e1)@[ABS]
              | Not e1->            (compile e1)@[NOT]
              | Plus (e1,e2)->      (compile e1)@(compile e2)@[PLUS]
              | Minus (e1,e2)->     (compile e1)@(compile e2)@[MINUS]
              | Mul (e1,e2)->       (compile e1)@(compile e2)@[MUL]
              | Div (e1,e2)->       (compile e1)@(compile e2)@[DIV]
              | Mod (e1,e2)->       (compile e1)@(compile e2)@[MOD]
              | Pow (e1,e2)->       (compile e1)@(compile e2)@[POW]
              | And (e1,e2)->       (compile e1)@(compile e2)@[AND]
              | Orr (e1,e2)->       (compile e1)@(compile e2)@[ORR]
              | Imply (e1,e2)->     (compile e1)@(compile e2)@[IMPLY]
              | Equal (e1,e2)->     (compile e1)@(compile e2)@[EQUAL]
              | Great (e1,e2)->     (compile e1)@(compile e2)@[GREAT]
              | Lesss (e1,e2)->     (compile e1)@(compile e2)@[LESSS]
              | Grequ (e1,e2)->     (compile e1)@(compile e2)@[GREQU]
              | Leequ (e1,e2)->     (compile e1)@(compile e2)@[LEEQU]
              | Projj (e1,List e2)->(compile (ith(e1,e2)));;

let rec map_ex f o = match o with
                        x::y->(f ([],rho,x))::(map_ex f y)
                    |   _->[];;
let rec execute (s,rho,opc) = match (s,opc) with
                        ([x], [])->                 x
                    |   (s, (CONST e)::c)->         execute((Int e)::s, rho, c)
                    |   (s, (TRUE)::c)->            execute((Bool true)::s, rho, c)
                    |   (s, (FALSE)::c)->           execute((Bool false)::s, rho, c)
                    |   (s, (VAR n)::c)->           execute((rho n)::s, rho, c)
                    |   (e::s, (ABS)::c)->          execute((abss e)::s, rho, c)
                    |   (e::s, (NOT)::c)->          execute((nott e)::s, rho, c)
                    |   (e2::e1::s, (PLUS)::c)->    execute((addd(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (MINUS)::c)->   execute((subb(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (MUL)::c)->     execute((mull(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (DIV)::c)->     execute((divv(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (MOD)::c)->     execute((modd(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (POW)::c)->     execute((expo(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (AND)::c)->     execute((andd(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (ORR)::c)->     execute((orrr(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (IMPLY)::c)->   execute((implyy(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (EQUAL)::c)->   execute((equall(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (GREAT)::c)->   execute((greatt(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (LESSS)::c)->   execute((lessss(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (GREQU)::c)->   execute((grequu(e1,e2))::s, rho, c)
                    |   (e2::e1::s, (LEEQU)::c)->   execute((leequu(e1,e2))::s, rho, c)
                    |   (s, (LIST e)::c)->          execute((Listans (map_ex execute e))::s,rho,c);;

(* Manual Test Cases for tuples *)
let a0=Div(Var "x",Const (-47));;
let a1=Leequ(Minus(Const (-14),Var "x"),Var "x");;
let a2=Abs(Var "x");;
let a3=Minus(Const (29),Var "x");;

let a = Projj(2, List([a0; a1; a2; a3]));;
(* 
# eval a;;
- : answer = Int 5
# execute ([],rho,compile a);;
- : answer = Int 5
 *)
let b = List([a0;a1;a2]);;
(* 
# eval b;;
- : answer = Listans [Int 0; Bool true; Int 5]
# execute ([],rho,compile b);;
- : answer = Listans [Int 0; Bool true; Int 5]
 *)

(* ****************************************************************** *)

(* Sample Tests with Answers *)
let a0=Mul(Plus(Pow(Var "x",Const (7)),Const (-57)),Var "x");;
(* 
# eval a0;;
- : answer = Int 390340
# execute ([],rho,compile a0);;
- : answer = Int 390340
 *)
let a1=Great(Mod(Const (-27),Const (-5)),Const (-27));;
(* 
# eval a1;;
- : answer = Bool true
# execute ([],rho,compile a1);;
- : answer = Bool true
 *)
let a2=Leequ(Mul(Pow(Div(Const (30),Var "x"),Const (35)),Var "x"),Var "x");;
(* 
# eval a2;;
- : answer = Bool true
# execute ([],rho,compile a2);;
- : answer = Bool true
 *)
let a3=Leequ(Plus(Var "x",Const (49)),Const (3));;
(* 
# eval a3;;
- : answer = Bool false
# execute ([],rho,compile a3);;
- : answer = Bool false
 *)

(* Test cases generated by python code*)
(* If required *)
