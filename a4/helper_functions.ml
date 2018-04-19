exception Undeclared_Variable;;

let rec poww a = function
                | 0 -> 1
                | 1 -> a
                | n -> 
                      let b = poww a (n/2) in
                      b*b*(if n mod 2 = 0 then 1 else a)
;;

let rec get_var x e = match e with
                      [] -> raise Undeclared_Variable
                    | (v,a)::t -> if v=x then a else get_var x t
;;

let rec map f a = match a with 
                    [x] -> [f x]
                |   x::y-> (f x)::(map f y)
;;

let rec ith (i,l) = match l with 
                    x::y-> if i=0 then x else ith(i-1,y)
;;

let remove x e = let rec del i l el = match l with 
                                        []->i 
                                    |   (h,s)::t-> if x=h then 
                                                    i@t 
                                               else 
                                                    del (i@[(h,s)]) t el
                 in del [] e x
;;