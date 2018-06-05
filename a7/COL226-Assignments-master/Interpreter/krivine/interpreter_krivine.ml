open Printf;;
open Types;;

let rec find_var var env = match env with
[] -> failwith "Undeclared variable accessed"
| (v,cl)::tl -> if v <> var then find_var var tl else (match cl with 
                                                        CL (FUNC x, env) -> CL (FUNC x, (v,cl)::env ) (* Modifying env to allow recursion*)
                                                      | _ -> cl
                                                      )
;;

let add_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (INTEGER (i1+i2),[])
| _ -> failwith "Invalid add operation"
;;

let mul_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (INTEGER (i1*i2),[])
| _ -> failwith "Invalid add operation"
;;

let sub_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (INTEGER (i1-i2),[])
| _ -> failwith "Invalid add operation"
;;

let equal_ arg1 arg2 = match (arg1,arg2) with
  (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (BOOL(i1 = i2),[])
| (CL (BOOL i1,env1) , CL (BOOL i2,env2) ) -> CL (BOOL(i1 = i2),[])
| _ -> failwith "Invalid add operation"
;;

let or_ arg1 arg2 = match (arg1,arg2) with
 (CL (BOOL i1,env1) , CL (BOOL i2,env2) ) -> CL (BOOL (i1||i2),[])
| _ -> failwith "Invalid add operation"
;;

let and_ arg1 arg2 = match (arg1,arg2) with
 (CL (BOOL i1,env1) , CL (BOOL i2,env2) ) -> CL (BOOL (i1&&i2),[])
| _ -> failwith "Invalid add operation"
;;

let less_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (BOOL (i1<i2),[])
| _ -> failwith "Invalid add operation"
;;

let grt_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (BOOL (i1>i2),[])
| _ -> failwith "Invalid add operation"
;;

let less_eq_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (BOOL (i1 <= i2),[])
| _ -> failwith "Invalid add operation"
;;

let grt_eq_ arg1 arg2 = match (arg1,arg2) with
 (CL (INTEGER i1,env1) , CL (INTEGER i2,env2) ) -> CL (BOOL (i1 >= i2),[])
| _ -> failwith "Invalid add operation"
;;

let if_else_ res_cl e1 e2 env = match res_cl with
 CL (BOOL i,_) -> if i then CL (e1,env) else CL (e2,env)
| _ -> failwith "Invalid If else operation"
;;

let exec_fn exec_cl delay_stk = match (exec_cl,delay_stk) with
  (_,[]) -> failwith "Invalid function call: Empty delay stack"
| (CL(FUNC (v,e),env),hd::tl) -> (CL(e,(v,hd)::env) , tl)
| _ -> failwith "Invalid exec_fn call: exec_cl is not of type function"
;;


let rec solve_krivine exec_cl delay_stack = match exec_cl with
  CL (NIL , env) -> CL (NIL,env)
| CL (INTEGER i,env) -> CL (INTEGER i, env)
| CL (BOOL i, env) -> CL (BOOL i, env)
| CL (VAR i, env) -> let new_cl = find_var (i) env in solve_krivine new_cl delay_stack

| CL ( MUL (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (mul_ res1 res2) delay_stack
| CL ( ADD (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (add_ res1 res2) delay_stack
| CL ( SUB (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (sub_ res1 res2) delay_stack
| CL ( EQUAL (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (equal_ res1 res2) delay_stack
| CL ( OR (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (or_ res1 res2) delay_stack
| CL ( AND (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (and_ res1 res2) delay_stack
| CL ( LESS (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (less_ res1 res2) delay_stack
| CL ( GRT (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (grt_ res1 res2) delay_stack
| CL ( LESS_EQ (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (less_eq_ res1 res2) delay_stack
| CL ( GRT_EQ (e1,e2) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in let res2 = solve_krivine (CL (e2,env)) [] in solve_krivine (grt_eq_ res1 res2) delay_stack

| CL ( IF_THEN_ELSE (e1,e2,e3) , env) -> let res1 = solve_krivine (CL (e1,env)) [] in solve_krivine (if_else_ res1 e2 e3 env) delay_stack

| CL (FUNC (v,e),env) -> let (cl,new_stk) = exec_fn exec_cl delay_stack in solve_krivine cl new_stk
| CL (APPLY (e1,e2),env) -> solve_krivine (CL (e1,env)) ((CL (e2,env))::delay_stack)
| CL (BIND (v,e),env) -> solve_krivine (CL (NIL,(v,CL (e,env))::env)) delay_stack
| _ -> failwith "Unsupported Instruction"
;;

let rec exec_krivine_prog prog env = match prog with
  [] -> Printf.printf "Program returned nothing\n"
| hd::tl -> let res_cl = solve_krivine (CL (hd,env)) [] in ( match res_cl with 
                                                             CL (NIL,env_new) -> exec_krivine_prog tl env_new
                                                           | CL (INTEGER i,_) -> Printf.printf "Result => %d\n" i
                                                           | CL (BOOL b,_) -> Printf.printf "Result => %B\n" b
                                                           | _ -> failwith "Invalid closure returned"
                                                           )
;;

(*
let x = 99;;
let foo = func x -> x + 4;;
let ans1 = (func x -> x + 9 ) (8+x);; (* ans1 = 116 *)
let ans2 = foo 9;; (* ans1 = 13 *)
ans1 + ans2 (* returns 129 *)
*)

let var_decl = BIND(Var "x", INTEGER 99);;
let fn_decl = BIND(Var "foo",FUNC (Var "x",ADD (VAR (Var "x"),INTEGER 4)));;
let anon_call = APPLY( FUNC (Var "x",ADD (VAR (Var "x"),INTEGER 9)) , (ADD (INTEGER 8 , VAR (Var "x") ) ) );;
let anon_call_2 = APPLY( VAR (Var "foo") , INTEGER 9);;
let ans1 = BIND(Var "ans1",anon_call);;
let ans2 = BIND(Var "ans2",anon_call_2);;
let final_exp = ADD(VAR(Var"ans1"),VAR(Var"ans2"));;
let prg = [var_decl;fn_decl;ans1;ans2;final_exp];;

let solve fl_name = 
  let decl = open_in fl_name in
  let lexbuf = Lexing.from_channel decl in
  let prog = Parser.main Lexer.translate lexbuf in
  exec_krivine_prog prog []
;;

let main () = let _ = solve "../test_prog.ml" in ()
;;

if !Sys.interactive then () else main ();;