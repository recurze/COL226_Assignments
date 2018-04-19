exception NoSuchElement;;
exception Not_Unifiable;;

let rec map f l initial = match l with
                []-> initial
            |   x::y-> map f y (initial@[f x]);;
(*
let rec reduce f l initial = match l with
                []->initial
            |   x::y-> reduce  f y (f x initial);;
 *)
let rec len li l = match li with
                    []->l
                |   x::y-> len y (l+1);;

let rec at m s = match m with
                    []->raise NoSuchElement
                |   (ss,a)::y-> if ss=s then a
                                else at y s;;

let sum a b = a+b;;
let andd a b = a && b;;
let orr a b = a || b;;
let append a b = a@b;;
let iszero (a,b) = (b=0);;

type variables = Var of string;;
type symbol = S of string;;
type signature = (symbol * int) list;;

type term = V of variables | Node of symbol*(term list);;
type sigma = (variables * term) list;;

let a_signature=[(S("x"),0); (S("y"),0); (S("+"),2); (S("-"),2); (S("fuckup"),-1); (S("rand"),3)];;
let b_signature=[(S("x"),0); (S("y"),0); (S("+"),2); (S("-"),2); (S("rand"),3)];;

let rec ispresent s si = match si with
                            []->false
                        |   (ss,a)::y-> if ss=s then true
                                        else ispresent s y;;
let check_sig si =
    let rec aux si ret= match si with
                            []-> ret
                        |   (s,a)::y-> if a<0 then false
                                       else if ispresent s y then false 
                                       else if a=0 then 
                                            aux y true
                                       else aux y ret in
    aux si false;;

let rec wfterm si t = match t with
                    V(Var(_))-> true
                |   Node(s,t1)-> 
                        if ((at si s) <> (len t1 0)) then false
                        else (List.fold_left  andd true (map (wfterm si) t1 []));;

let rec ht t = match t with 
                    V(Var(_))-> 0
                |   Node(s,t1)-> 1+List.fold_left max 0 (map ht t1 []);;

let rec size t = match t with 
                    V(Var(_))->1
                |   Node(s,t1)-> 1+List.fold_left sum 0 (map size t1 []);;

let rec present_list s l = match l with 
                            []->false
                        |   x::y-> if x=s then true else present_list s y;;

let rec union y x = match y with 
                    []->x
                |   h::t-> if present_list h x then union t x
                           else union t (h::x);;

let rec vars t = match t with 
                    V(x)-> [x]
                |   Node(s,t1)-> List.fold_left union [] (map vars t1 []);;

let compose sigma1 sigma2 = sigma1 @ sigma2;;

let rec subst sigma t = match t with 
                        V(x)-> if (ispresent x sigma) then (final sigma x)
                              else V(x)
                    |   Node(s,t1) -> Node (s,map (subst sigma) t1 [])

and final sigma x = let ret = at sigma x in
                    match ret with 
                        V(Var("x"))->subst sigma (V(Var("x")))
                    |   _ -> ret;;

let rec mgu term1 term2 = match (term1,term2) with 
                        (V(x),V(y))-> if x=y then [] else [(x,V(y))]
                    |   (V(x),Node(s,t1)) | (Node(s,t1),V(x))-> 
                            if present_list x (vars (Node(s,t1))) then 
                                raise Not_Unifiable
                            else
                                [(x,Node(s,t1))]
                    |   (Node(s1,t1),Node(s2,t2))-> 
                            if s1<>s2 then raise Not_Unifiable
                            else begin
                                let rec aux si l1 l2= match (l1,l2) with 
                                        ([],[])->si
                                    |   (h::t,hh::tt)-> let t4 = mgu (subst si h) (subst si hh) in
                                                        aux (compose si t4) t tt
                                in
                                aux [] t1 t2
                            end;;

let t=Node(S("+"),[V (Var ("xy")) ; Node(S("-"),[V (Var("y")); V (Var("y"))])]);;
let t1=Node(S("+"),[V (Var ("xy")) ; Node(S("-"),[V (Var("x")); Node(S("x"),[])])]);;

let t1=Node(S("f"),[Node(S("h"),[Node(S("a"),[]); V(Var("y"))]); V(Var("x"))]);;
let t2=Node(S("f"),[V(Var("x")); Node(S("h"),[Node(S("b"),[]); V(Var("y"))])]);;
(* mgu t1 t2;; *)
(* Exception: Not_Unifiable. *)

let x1=Node(S("+"),[Node(S("-"),[V(Var("x"));V(Var("y"))]);Node(S("+"),[V(Var("x"));Node(S("a"),[])])]);;
let x2=Node(S("+"),[Node(S("-"),[V(Var("x"));V(Var("y"))]);Node(S("a"),[])]);;
(* mgu x1 x2;; *)
(* Exception: Not_Unifiable. *)

let x3=Node(S("+"),[Node(S("-"),[V(Var("x"));V(Var("y"))]);Node(S("-"),[V(Var("x"));Node(S("a"),[])])]);;
(* mgu x1 x3;; *)
(* Exception: Not_Unifiable. *)


let x4=Node(S("+"),[Node(S("-"),[V(Var("x"));V(Var("x"))]);Node(S("+"),[V(Var("x"));Node(S("a"),[])])]);;
let x5=Node(S("+"),[Node(S("-"),[Node(S("a"),[]);V(Var("x"))]);Node(S("+"),[V(Var("x"));Node(S("a"),[])])]);;
mgu x4 x5;;
(* (variables * term) list = [(Var "x", Node (S "a", []))] *)


if (subst (mgu x4 x5) x4)=(subst (mgu x4 x5) x5) then print_string "correct" else print_string "Incorrect";;
(* correct- : unit = () *)

subst (mgu x4 x5) x4;;
(* term = Node (S "+", [Node (S "-", [Node (S "a", []); Node (S "a", [])]);
  Node (S "+", [Node (S "a", []); Node (S "a", [])])]) *)

let x6=Node(S("+"),[Node(S("-"),[V(Var("x"));V(Var("y"))]);Node(S("+"),[V(Var("x"));Node(S("a"),[])])]);;
mgu x6  x5;;
(* (variables * term) list = [(Var "x", Node (S "a", [])); (Var "y", Node (S "a", []))] *)

subst (mgu x6 x5)  x5;;
(*  term =
Node (S "+",
 [Node (S "-", [Node (S "a", []); Node (S "a", [])]);
  Node (S "+", [Node (S "a", []); Node (S "a", [])])])
 *)

if (subst (mgu x6 x5) x5)=(subst (mgu x6 x5) x6) then print_string "correct" else print_string "incorrect";;
(* correct- : unit = () *)
