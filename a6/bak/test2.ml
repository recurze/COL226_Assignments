let fact1 = Fact(
                Node(
                    S("child", 2),[
                        Const("Luke");
                        Const("Vader")
                    ]
                )
            )
;;
let fact2 = Fact(
                Node(
                    S("child", 2),[
                        Const("Leia");
                        Const("Vader")
                    ]
                )
            )
;;

let rule = Rule(
                Node(
                    S("father", 2), [
                        V("x");
                        V("y")
                    ]
                ),

                [ Node(
                    S("child", 2), [
                        V("y");
                        V("x")
                    ]
                  )
                ]
            )
;;


let p = [fact1; fact2; rule];;
let g = [Node(S("father", 2), [Const("Vader"); V("b")])];;

