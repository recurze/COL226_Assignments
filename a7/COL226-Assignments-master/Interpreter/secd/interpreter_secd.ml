open Printf;;
open Types;;


let add stk = match stk with
  (VCL_INT x )::(VCL_INT y :: tl) -> (VCL_INT (x+y))::tl
| _ -> failwith "Wrong Stack"
;;

let less stk = match stk with
  (VCL_INT y )::(VCL_INT x :: tl) -> (VCL_BOOL (x<y))::tl
| _ -> failwith "Wrong Stack"
;;
let grt stk = match stk with
  (VCL_INT y )::(VCL_INT x :: tl) -> (VCL_BOOL (x>y))::tl
| _ -> failwith "Wrong Stack"
;;
let less_eq stk = match stk with
  (VCL_INT y )::(VCL_INT x :: tl) -> (VCL_BOOL (x<=y))::tl
| _ -> failwith "Wrong Stack"
;;
let grt_eq stk = match stk with
  (VCL_INT y )::(VCL_INT x :: tl) -> (VCL_BOOL (x>=y))::tl
| _ -> failwith "Wrong Stack"
;;

let sub stk = match stk with
  (VCL_INT y )::(VCL_INT x :: tl) -> (VCL_INT (x-y))::tl
| _ -> failwith "Wrong Stack"
;;

let mul stk = match stk with
  (VCL_INT x )::(VCL_INT y :: tl) -> (VCL_INT (x*y))::tl
| _ -> failwith "Wrong Stack"
;;

let equal stk = match stk with
  (VCL_INT x )::(VCL_INT y :: tl) -> (VCL_BOOL (x=y))::tl
| (VCL_BOOL x )::(VCL_BOOL y :: tl) -> (VCL_BOOL (x=y))::tl
| _ -> failwith "Wrong Stack"
;;


let _and stk = match stk with
  (VCL_BOOL x )::(VCL_BOOL y :: tl) -> (VCL_BOOL (x&&y))::tl
| _ -> failwith "Wrong Stack"
;;

let _or stk = match stk with
  (VCL_BOOL x )::(VCL_BOOL y :: tl) -> (VCL_BOOL (x||y))::tl
| _ -> failwith "Wrong Stack"
;;

let assign_var var env stk = match stk with
  hd::tl -> ((var,hd)::env , tl)
| [] -> failwith "Empty Stack"
;;

let pop_var env= match env with
  hd::tl -> tl
| [] -> failwith "Empty Environment"
;;

let func_call stk = match stk with
  (CL (env,opl) )::tl -> (env,opl,tl)
| _ -> failwith "Wrong stack : Invalid function call"
;;

let func_ret dmp = match dmp with
  (env,opl) ::tl -> (env,opl,tl)
| _ -> failwith "Wrong dump : Invalid function return"
;;

let rec find_var var env = match env with
[] -> failwith "Undeclared variable accessed"
| (v,cl)::tl -> if v <> var then find_var var tl else (match cl with 
                                                      CL (env,oplist) -> CL ((var,cl)::env,oplist) (* Modifying env to allow recursion*)
                                                      | _ -> cl
                                                      )
;;

let if_else stk opl1 opl2 = match stk with
(VCL_BOOL b)::tl -> if b then (opl1,tl) else (opl2,tl)
| _ -> failwith "Wrong stack: Invalid if statement"

let rec interpret_secd oplist env stk dmp = match oplist with
  [] -> stk
| (INTEGER i) :: tl -> interpret_secd tl env ((VCL_INT i)::stk) dmp
| (BOOL i) :: tl -> interpret_secd tl env ((VCL_BOOL i)::stk) dmp
| (ADD ) :: tl -> interpret_secd tl env (add stk) dmp
| (SUB ) :: tl -> interpret_secd tl env (sub stk) dmp
| (LESS ) :: tl -> interpret_secd tl env (less stk) dmp
| (GRT ) :: tl -> interpret_secd tl env (grt stk) dmp
| (LESS_EQ ) :: tl -> interpret_secd tl env (less_eq stk) dmp
| (GRT_EQ ) :: tl -> interpret_secd tl env (grt_eq stk) dmp
| (OR ) :: tl -> interpret_secd tl env (_or stk) dmp
| (AND ) :: tl -> interpret_secd tl env (_and stk) dmp
| (MUL ) :: tl -> interpret_secd tl env (mul stk) dmp
| (EQUAL ) :: tl -> interpret_secd tl env (equal stk) dmp
| (LET v) :: tl -> let (new_env,new_stk) = assign_var v env stk in interpret_secd tl new_env new_stk dmp
| (ENDLET ) :: tl -> let new_env = pop_var env in interpret_secd tl new_env stk dmp
| (CLOSURE opl ) :: tl -> interpret_secd tl env (CL (env,opl) ::stk) dmp
| (APPLY ) :: tl -> let new_env,new_opl,new_stk = func_call stk in interpret_secd new_opl new_env new_stk ((env,tl) ::dmp)
| (RETURN ) :: tl -> let new_env,new_opl,new_dmp = func_ret dmp in interpret_secd new_opl new_env stk new_dmp
| (ACCESS v) :: tl -> let cl = find_var v env in interpret_secd tl env (cl::stk) dmp
| (IF_THEN_ELSE (opl1,opl2)) :: tl -> let opl_new,new_stk = if_else stk opl1 opl2 in interpret_secd (opl_new @ tl) env new_stk dmp
;;

let y = Var "y";;
let x = Var "x";;
let func_call = Var "func_call";;
let y_44 = [INTEGER(44);LET(y)];;
let fn_decl = [CLOSURE([LET(x);INTEGER(3);ACCESS(x);ADD;ACCESS(y);ADD;RETURN]);LET(func_call)];;
let y_99 = [INTEGER(99);LET(y)];;
let fnc_call = [INTEGER(4);ACCESS(func_call);APPLY];;
let bind_x = [LET(x)];;
let add_x_y = [ACCESS(x);ACCESS(y);ADD];;
let prog = y_44@fn_decl@y_99@fnc_call@bind_x@add_x_y;;


let print_stack_top stk = match stk with
  [] -> Printf.printf "Empty stack returned\n"
| (VCL_INT i)::tl -> Printf.printf "Result => %d\n" i
| (VCL_BOOL b)::tl -> Printf.printf "Result => %B\n" b
| _ -> Printf.printf "Element on top of stack is not a Value Closure\n"
;;

let solve fl_name = 
  let decl = open_in fl_name in
  let lexbuf = Lexing.from_channel decl in
  let prog = Parser.main Lexer.translate lexbuf in
  print_stack_top (interpret_secd prog [] [] [])
;;

let main () = let _ = solve "../test_prog.ml" in ()
;;

if !Sys.interactive then () else main ();;
