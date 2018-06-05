let x = 99;;
let foo = func x -> (x + 4);;
let ans1 = func x -> (x + 9 ) (8+x);; (* ans1 = 116 *)
let ans2 = foo (9);; (* ans1 = 13 *)
(*ans1 + ans2 ;;  returns 129 *)

let y = 33;;
let func_call = func x -> (x+3+y);;
let y = 99;;
(*func_call (8);;*)

let fact = func x -> (if(x <= 1) then (1) else (x*(fact(x-1))) );;
fact( 10 ) < 590;;

let fib = func x -> (if (x <= 0 ) then 
                        (0-1)
                     else 
                        ( if ( (x = 1) || (x = 2) ) then
                              (1)
                          else
                              ( (fib (x-1)) + (fib (x-2)) )
                        )
                    )
;;

(*fib (0-5);;*)

let f1 = func x -> (func y -> (x + y) );;
let f2 = f1 ( 2 );;
let f3 = f1 ( 9 );;
(*f2 (1);;*)
(*f3 (1);;*)