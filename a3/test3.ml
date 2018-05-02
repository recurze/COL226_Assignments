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
