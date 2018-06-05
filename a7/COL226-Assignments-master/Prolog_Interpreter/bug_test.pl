let atm = Atom (Sym "concat", [Cons "nil"; Var "_A"; Var "_A"])
let g_1 = Atom (Sym "concat", [Cons "nil"; Node (Atom (Sym "con", [Cons "a"; Cons "nil"])); Var "X"])
let unifier = idty_subst;;

mgu_atm atm g_1;;

let s = unifier;;
let lst1 = [Cons "nil"; Var "_A"; Var "_A"];;
let lst2 = [Cons "nil"; Node (Atom (Sym "con", [Cons "a"; Cons "nil"])); Var "X"];;

List.fold_left2 (fun s a b -> compose (mgu (subst s a) (subst s b)) s ) idty_subst lst1 lst2;;

let s = idty_subst;;

let a = Cons "nil";;
let b = Cons "nil";;
let s = compose (mgu (subst s a) (subst s b)) s;;

let a =  Var "_A";;
let b =  Node (Atom (Sym "con", [Cons "a"; Cons "nil"]));;
let s = compose (mgu (subst s a) (subst s b)) s;;

let a = Var "_A";;
let b = Var "X";;

(compose (mgu_atm (subst_atm unifier atm) (subst_atm unifier g_1)) unifier )




let t= Node (Atom (Sym "con", [Cons "a"; Cons "nil"]));;
let y = Var "X";;
mgu t y;;
