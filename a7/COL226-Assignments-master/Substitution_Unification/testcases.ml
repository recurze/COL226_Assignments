#use "assignment4.ml";;
let t1 = (V(Var("x")));;
let t2 = (V(Var("y")));;
let t3 = ( Node( Sym("a") , [V(Var("x"))] ) );;
let t4 = ( Node( Sym("b") , [V(Var("x"))] ) );;
let t5 = ( Node( Sym("a") , [V(Var("x")) ; V(Var("x"))] ) );;
let t6 = ( Node( Sym("a") , [V(Var("x")) ; V(Var("z"))] ) );;
let t7 = ( Node( Sym("a") , [V(Var("y")) ; V(Var("z"))] ) );;
let t8 = ( Node( Sym("b") , [V(Var("y")) ; V(Var("z"))] ) );;
let t9 = ( Node( Sym("a") , [] ) );;
let t0 = ( Node( Sym("b") , [] ) );;
let t11 = ( Node( Sym("p") , [ Node(Sym("a"),[V(Var("x"));V(Var("x"))]) ; V(Var("y"))] ) );;
let t12 = ( Node( Sym("p") , [ Node(Sym("a"),[V(Var("a"));V(Var("Z"))]) ; V(Var("b"))] ) );;

let t13 = (Node (Sym("f"), [Node(Sym("a"),[]);Node( Sym("b"),[ (V(Var("var2"))) ; Node(Sym("c"),[Node(Sym("d"),[])])] ) ] ));;
let t14 = (Node (Sym("f"), [V(Var("var1"));Node( Sym("b"),[ (V(Var("var2"))) ; Node(Sym("c"),[V(Var("var3"))])] ) ] ));;

let sig1 = [(Sym("x"),2)];;
let sig2 = [(Sym("x"),-2)];;
let sig3 = [(Sym("a"),2);(Sym("b"),0)];;
let sig4 = [(Sym("a"),1);(Sym("b"),2)];;
let sig5 = [(Sym("x"),2);(Sym("y"),2);(Sym("z"),0)];;

check_sig sig1;;
check_sig sig2;;
check_sig sig3;;
check_sig sig4;;
check_sig sig5;;

wfterm sig1 t1;;
wfterm sig1 t3;;
wfterm sig3 t3;;
wfterm sig3 t5;;
wfterm sig3 t0;;

ht t1;;
size t1;;
print_vars t1;;

ht t2;;
size t2;;
print_vars t2;;

ht t3;;
size t3;;
print_vars t3;;

ht t4;;
size t4;;
print_vars t4;;

ht t5;;
size t5;;
print_vars t5;;

ht t6;;
size t6;;
print_vars t6;;

ht t7;;
size t7;;
print_vars t7;;

ht t8;;
size t8;;
print_vars t8;;

ht t9;;
size t9;;
print_vars t9;;

ht t0;;
size t0;;
print_vars t0;;

let x = mgu t1 t2 in
print_subst t1 t2 x;;

let x = mgu t3 t4 in
print_subst t3 t4 x;;

let x = mgu t5 t5 in 
print_subst t5 t5 x;;

let x = mgu t1 t6 in
print_subst t1 t6 x;;

let x = mgu t8 t7 in
print_subst t8 t7 x;;

let x = mgu t9 t9 in
print_subst t9 t9 x;;

let x = mgu t0 t9 in
print_subst t0 t9 x;;

let x = mgu t11 t12 in
print_subst t11 t12 x;;

let x = mgu t13 t14 in
print_subst t13 t14 x;;