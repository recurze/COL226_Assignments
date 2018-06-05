exception NOT_UNIFIABLE;;
module SS = Set.Make(String);;
type variable = Var of string;;
type symbol = Sym of string;;
type term = V of variable | Node of symbol*(term list);;

let rec _check_sig sym_list = function
    [] -> true
|   (Sym(sym),arity)::rest -> if (arity >= 0) && ((List.exists (fun sy -> sy=sym) sym_list) = false) then (_check_sig (sym::sym_list) rest) else false
;;
let check_sig signature = _check_sig [] signature;;

let rec wfterm signature = function
    V(x) -> true
|   Node(s,lst) -> try let (_,arity) = List.find (fun (x,arity) -> x = s) signature in if (List.length lst) = arity then (List.for_all (wfterm signature) lst) else false
                   with Not_found -> false
;;

let rec ht = function
    V(x) -> 1
|   Node(sym,lst) -> List.fold_left (fun a b -> max a ((ht b) + 1)) 1 lst
;;

let rec sz acc_size = function
    V(x) -> 1+acc_size
|   Node(sym,lst) -> List.fold_left (fun a b -> sz a b) (acc_size+1) lst
;;
let size x = sz 0 x;;

let rec _vars set = function
    V(Var(x)) -> SS.add x set
|   Node(sym,args) -> List.fold_left (fun a b -> _vars a b) set args
;;
let vars term = _vars SS.empty term;;

let rec subst sigma = function
    V(x) -> sigma x
|   Node(sym,lst) -> Node(sym, List.map (subst sigma) lst )
;;

(* Evaluates sigma1 ( sigma2 ) *)
let rec compose sigma1 sigma2 = fun x -> subst sigma1 (sigma2 x );;

let idty_subst = fun x -> V(x) ;;

let rec mgu t1 t2 = match (t1,t2) with
    (Node(x,lst1),Node(y,lst2)) -> if (x <> y) || ((List.length lst1) <> (List.length lst2)) then raise NOT_UNIFIABLE
                                   else List.fold_left2 (fun s a b -> compose (mgu (subst s a) (subst s b)) s ) idty_subst lst1 lst2
|   (V(x),V(y)) -> fun var -> if var = x then (V y) else (V var)
|   (V(x),Node(f,lst)) -> if SS.exists (fun a -> Var(a)=x) (vars (Node (f,lst))) then raise NOT_UNIFIABLE else (fun var -> if var = x then (Node(f,lst)) else (V var) )
|   (Node(f,lst),V(x)) -> mgu (V x) (Node (f,lst))
;;

let print_subst t1 t2 subst = let var1 = vars t1 in let var2 = _vars var1 t2 in SS.fold (fun x lst -> (Var(x),(subst (Var x)))::lst ) var2 [];; 
let print_vars t = let v = vars t in SS.fold (fun x lst -> (Var x)::lst ) v [];;