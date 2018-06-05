let f1 = Node(S("teacher", 2), [Const "fred"; Const "history"]);;
let f2 = Node(S("teacher", 2), [Const "fred"; Const "english"]);;
let f3 = Node(S("teacher", 2), [Const "fred"; Const "drama"]);;
let f4 = Node(S("teacher", 2), [Const "fred"; Const "physics"]);;

let f5 = Node(S("studies", 2), [Const "Alice"; Const "english"]);;
let f6 = Node(S("studies", 2), [Const "Angus"; Const "english"]);;
let f7 = Node(S("studies", 2), [Const "amelia"; Const "drama"]);;
let f8 = Node(S("studies", 2), [Const "alex"; Const "physics"]);;

let p = [Fact(f1); Fact(f2); Fact(f3); Fact(f4); Fact(f5); Fact(f6); Fact(f7); Fact(f8)];;
let g = [Node(S("teacher", 2), [Const "fred"; V "Course"]);Cut;Node(S("studies",2), [V "Student";V "Course"]) ];;

(*let g = [Node(S("teacher", 2), [Const "fred"; Const "history"]);Node(S("studies",2), [Const "alex";Const "physics"]) ];;
*)
