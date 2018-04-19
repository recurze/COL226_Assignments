let e = Apply_e1_e2(Lambda("x", Add(V "x", C 3)) , C 4);;
krivine(CLOS([],e),[]);;
execute ([], [], (compile e), []);;

let e = Apply_e1_e2(
            Lambda("x", 
                Apply_e1_e2(Lambda("y", 
                    Add(V "y", V "x")
                ), C 3)
            ), C 4)
;; 
krivine(CLOS([],e),[]);;
execute ([], [], (compile e), []);;

(* If_Then_Else *)

let e = If_then_else(Eql(C 4, Add(C 2, C 1)), 
  Apply_e1_e2(Lambda("x", Add(V "x", C 3)) , C 4), 
  Apply_e1_e2(Lambda("x", Add(V "x", C 5)) , C 6)
  );;
krivine(CLOS([],e),[]);;
execute ([], [], (compile e), []);;


let e = Add(C 4, If_then_else(Eql(C 7, C 7), C 1, Booleans(true)));;
krivine(CLOS([],e),[]);;
execute ([], [], (compile e), []);;
