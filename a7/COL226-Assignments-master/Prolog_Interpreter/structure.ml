open Printf;;
open Unix;;

type symbol = Sym of string;;
type term = Var of string | Cons of string | Node of atom
and atom = Atom of symbol * (term list);;
type goal = atom list;;
type body = Body of atom list;;
type head = Head of atom;;
type clause = Fact of head | Rule of head * body;;
type program = clause list;;
type 'a answer = True of 'a | False;;

exception NOT_WELLFORMED;;
exception NOT_UNIFIABLE;;
module SS = Set.Make(String);;

let rec find_arity sym = function
  [] -> raise Not_found
| (sy,arity)::tl -> if sy = sym then arity else find_arity sym tl
;;

let rec wfatm signature (Atom (sy,trm_lst)) = let lst_len = List.length trm_lst in
                                   let (new_sig,arity) = try (signature,(find_arity sy signature)) with Not_found -> (((sy,lst_len)::signature),lst_len) in
                                   if (arity <> lst_len) then raise NOT_WELLFORMED else List.fold_left (fun sign trm -> wfterm sign trm ) new_sig trm_lst 
and wfterm signature = function
  Var (str) -> signature
| Cons (str) -> signature
| Node (atm) -> wfatm signature atm
;;

let rec wfprog signature = function
  [] -> true
| hd::tl -> (
              match hd with
                Fact (Head atm) -> wfprog (wfatm signature atm) tl
              | Rule ((Head atm),(Body atm_list)) -> wfprog (wfatm signature atm) ((List.map (fun x -> (Fact (Head x))) atm_list)@tl)
            )
;;

let rec _vars set = function
    Var(x) -> SS.add x set
|   Cons(x) -> set
|   Node(Atom(sym,args)) -> List.fold_left (fun a b -> _vars a b) set args
;;
let vars term = _vars SS.empty term;;

let vars_atm set (Atom (_,trm_list)) = List.fold_left (fun a b -> _vars a b) set trm_list;;

(*
  (bytes -> term) -> term -> term = <fun>
*)
let rec subst sigma = function
    Var(x) -> sigma x
|   Cons(x) -> Cons(x)
|   Node(Atom(sym,lst)) -> Node(Atom(sym, List.map (subst sigma) lst ) )
;;

let subst_atm sigma (Atom (sy,lst) ) = Atom( sy, List.map (subst sigma) lst);;

(* Evaluates sigma1 ( sigma2 ) *)
let rec compose sigma1 sigma2 = fun x -> subst sigma1 (sigma2 x );;

(*bytes -> term = <fun> *)
let idty_subst = fun x -> Var(x) ;;

let rec mgu t1 t2 = match (t1,t2) with
    ( (Node (Atom(x,lst1)) ), (Node (Atom(y,lst2)) ) ) -> if (x <> y) || ((List.length lst1) <> (List.length lst2)) then raise NOT_UNIFIABLE
                                   else List.fold_left2 (fun s a b -> compose (mgu (subst s a) (subst s b)) s ) idty_subst lst1 lst2
|   (Var(x),Var(y)) -> fun var -> if var = x then (Var y) else (Var var)
|   (Var(x),Cons(y)) -> fun var -> if var = x then (Cons y) else (Var var)
|   (Cons(x),Var(y)) -> mgu (Var y) (Cons x)
|   (Cons(x),Cons(y)) -> if (x <> y) then raise NOT_UNIFIABLE else idty_subst
|   (Var(x),Node(Atom(f,lst))) -> if SS.exists (fun a -> a=x) (vars (Node ( Atom(f,lst) ) ) ) then raise NOT_UNIFIABLE else (fun var -> if var = x then (Node(Atom(f,lst))) else (Var var) )
|   (Node(Atom(f,lst)),Var(x)) -> if SS.exists (fun a -> a=x) (vars (Node ( Atom(f,lst) ) ) ) then raise NOT_UNIFIABLE else (fun var -> if var = x then (Node(Atom(f,lst))) else (Var var) )
|   _ -> raise NOT_UNIFIABLE
;;

let mgu_atm (Atom (sy1,lst1)) (Atom (sy2,lst2) ) =  if (sy1 <> sy2) || ((List.length lst1) <> (List.length lst2)) then raise NOT_UNIFIABLE
                                   else List.fold_left2 (fun s a b -> compose (mgu (subst s a) (subst s b)) s ) idty_subst lst1 lst2
;;


let print_subst t1 t2 subs = let var1 = vars t1 in let var2 = _vars var1 t2 in SS.fold (fun x lst -> (Var(x),(subs x))::lst ) var2 [];; 
let print_vars t = let v = vars t in SS.fold (fun x lst -> (Var x)::lst ) v [];;


let rec find_feasible fn = function
  [] -> (* Printf.printf "failed find_feasible"; flush Pervasives.stdout; *) False
| hd::tl -> (
                match (fn hd) with
                  False -> find_feasible fn tl
                | True ans -> (True ans)
              )
;;

(*
  Modify_prog changes the variable names in the prog to _+<var_name>
  This is done to ensure that variables names do not mix during unification
*)

let rec modify_prog program = function
  [] -> List.rev program
| cl :: tl -> modify_prog ((modify_clause cl)::program) tl
and modify_clause = function
| Fact ( Head atm ) -> (Fact (Head (modify_atm atm)) )
| Rule (( Head head_atm ),( Body atm_list )) -> Rule ((Head (modify_atm head_atm)),(Body (List.map modify_atm atm_list) ))
and modify_atm (Atom (sy,term_list)) = (Atom (sy, List.map modify_term term_list) )
and modify_term = function
| Var s -> Var ("_"^s)
| Cons s -> Cons s
| Node at -> Node (modify_atm at)
;;

let rec string_of_atom (Atom ((Sym sy),trm_list)) = let base = Printf.sprintf "%s( " sy in
                                                    Printf.sprintf "%s)" (List.fold_left (fun a b -> Printf.sprintf "%s%s, " a (string_of_term b) ) base trm_list)
and string_of_term trm = match trm with
  Var str -> Printf.sprintf "Var(%s)" str
| Cons str -> Printf.sprintf "Cons(%s)" str
| Node atm -> Printf.sprintf "(%s)" (string_of_atom atm)
;;

let get1char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char Pervasives.stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let rec continue_answer () = let () = flush Pervasives.stdout in let ch = get1char() in let () = Printf.printf "\n" in let () = flush Pervasives.stdout in if ch = ';' then true else if ch = '.' then false else
   let () = Printf.printf "Unrecognized option.\nEnter \';\' to continue backtracking \nor enter \'.\' to terminate." in let () = flush Pervasives.stdout in continue_answer()
;;
let print_term trm = Printf.printf "%s " (string_of_term trm);;
let print_ans var unif = SS.iter (fun a -> Printf.printf "%s = " a ; print_term (unif a) ; Printf.printf " | " ) var



let rec solve is_neg var unifier program goals = match goals with
  [] -> if(is_neg) then True unifier else (print_ans var unifier;flush Pervasives.stdout; if (continue_answer () ) then False else True unifier)
| (Atom (Sym("$eq"),[t1;t2]) ) :: g_tail -> (
                        try
                          let unif2 = (compose (mgu (subst unifier t1) (subst unifier t2)) unifier)
                          in solve is_neg var unif2 program g_tail
                        with
                          NOT_UNIFIABLE -> False
                        )
| (Atom (Sym("$neq"),[t1;t2]) ) :: g_tail -> (
                        try
                          let _ = (compose (mgu (subst unifier t1) (subst unifier t2)) unifier)
                          in False
                        with
                          NOT_UNIFIABLE -> solve is_neg var unifier program g_tail
                        )
| (Atom (Sym("$not"),[Node(t1)]) ) :: g_tail -> (
                                            match (solve (not is_neg) var unifier program (t1::g_tail)) with 
                                              False -> solve is_neg var unifier program g_tail
                                            | True _ -> False
                                          )
| g_head :: g_tail -> 
            let new_prog = (modify_prog [] program) in
            find_feasible (fun clause -> try (solve_clause is_neg var unifier new_prog goals clause) with NOT_UNIFIABLE -> False ) new_prog

and solve_clause is_neg var unifier program (g_1::g_rest) clause = match clause with
  Fact (Head atm) -> let unif2 = (compose (mgu_atm (subst_atm unifier atm) (subst_atm unifier g_1)) unifier ) in solve is_neg var unif2 program g_rest
| Rule ((Head atm),(Body atm_list)) -> let unif2 = (compose (mgu_atm (subst_atm unifier atm) (subst_atm unifier g_1)) unifier ) in solve is_neg var unif2 program (atm_list @ g_rest)
;;
