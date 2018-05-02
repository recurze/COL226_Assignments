let addcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], Const(aa1+aa2))
;;

let subcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], Const(aa1-aa2))
;;

let divcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], Const(aa1/aa2))
;;

let mulcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], Const(aa1*aa2))
;;

let modcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], Const(aa1 mod aa2))
;;

let abscl a1 =
    match a1 with
        CLOSURE(t1, Const aa1) ->
            if aa1<0 then
                CLOSURE([], Const (-aa1))
            else
                CLOSURE([], Const aa1)
;;

let notcl a1 =
    match a1 with
        CLOSURE(t1, True) -> CLOSURE([], False)
    |   CLOSURE(t1, False) -> CLOSURE([], True)
;;

let andcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, True), CLOSURE(t2, True)) ->
            CLOSURE([], True)
        |   _ -> CLOSURE([], False)
;;

let orrcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, False), CLOSURE(t2, False)) ->
            CLOSURE([], False)
    |   _ -> CLOSURE([], True)
;;

let implycl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, True), CLOSURE(t2, False)) ->
            CLOSURE([], False)
        |   _ -> CLOSURE([], True)
;;

let greatcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], if (aa1>aa2) then True else False)
;;

let equalcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], if (aa1=aa2) then True else False)
;;

let lessscl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], if (aa1<aa2) then True else False)
;;

let grequcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], if (aa1>=aa2) then True else False)
;;

let leequcl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], if (aa1<=aa2) then True else False)
;;

let rec expocl (a1, a2) =
    match (a1, a2) with
        (CLOSURE(t1, Const aa1), CLOSURE(t2, Const aa2)) ->
            CLOSURE([], Const (poww aa1 aa2))
;;

let rec krivine c s =
    match (c, s) with
        (CLOSURE(gamma, True), s) ->
            CLOSURE(gamma, True)
    |   (CLOSURE(gamma, False), s) ->
            CLOSURE(gamma, False)
    |   (CLOSURE(gamma, Const(n)), s) ->
            CLOSURE(gamma, Const(n))
    |   (CLOSURE(gamma, Abs(n)), s) ->
            CLOSURE(gamma, Abs(n))
    |   (CLOSURE(gamma, Not(n)), s) ->
            CLOSURE(gamma, Not(n))

    |   (CLOSURE(gamma, Projj(i, List(ex))), s) ->
            krivine (CLOSURE(gamma, ith(i, ex))) []
    |   (CLOSURE(gamma, List(l)), s) -> CLOSURE(gamma, List(l))
    |   (CLOSURE(gamma, Plus(n, m)), s) ->
            krivine (addcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s
    |   (CLOSURE(gamma, Minus(n, m)), s) ->
            krivine (subcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Mul(n, m)), s) ->
            krivine (mulcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Div(n, m)), s) ->
            krivine (divcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Mod(n, m)), s) ->
            krivine (modcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Pow(n, m)), s) ->
            krivine (expocl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, And(n, m)), s) ->
            krivine (andcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Orr(n, m)), s) ->
            krivine (orrcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Imply(n, m)), s) ->
            krivine (implycl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Equal(n, m)), s) ->
            krivine (equalcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s


    |   (CLOSURE(gamma, Great(n, m)), s) ->
            krivine (greatcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Lesss(n, m)), s) ->
            krivine (lessscl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Grequ(n, m)), s) ->
            krivine (grequcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Leequ(n, m)), s) ->
            krivine (leequcl  (
                            (krivine (CLOSURE(gamma, n)) []),
                            (krivine (CLOSURE(gamma, m)) [])
                            )
                    ) s

    |   (CLOSURE(gamma, Var(x)), s) ->
        krivine (get_var (Var x) gamma) s

    |   (CLOSURE(gamma, Lambda(y, e)), cl::ss) ->
        krivine (CLOSURE((y,cl)::gamma, e)) ss

    |   (CLOSURE(gamma, LetInEnd(y, e1, e2)), s) ->
        (let temp = krivine (CLOSURE((y,(krivine (CLOSURE(gamma, e1)) []))::gamma, e2)) s in
        match temp with CLOSURE(t, e) -> CLOSURE(remove y t, e))

    |   (CLOSURE(gamma, Apply(f, e)), s) ->
        krivine (CLOSURE(gamma, f)) (CLOSURE(gamma, e)::s)

    |   (CLOSURE(gamma, Ite(e, et, ef)), s) ->
        krivine (match (krivine (CLOSURE(gamma, e)) []) with
                        CLOSURE(_, True) -> CLOSURE(gamma, et)
                    |   CLOSURE(_, False) -> CLOSURE(gamma, ef)
        ) s
;;

let rec map_kr l env =
    match l with
        [] -> []
    |   x::y ->
            (krivine (CLOSURE(env, x)) [])::(map_kr y env)
;;

let rec map_cl l env =
    match l with
        [] -> []
    |   x::y ->
            CLOSURE(env, x)::(map_cl y env)
;;

let rec give_me_answers cl =
    match cl with
        CLOSURE(_, True) -> Bool(true)
    |   CLOSURE(_, False) -> Bool(false)
    |   CLOSURE(_, Const(n)) -> Int(n)
    |   CLOSURE(e, List(l)) ->
            Listans (map give_me_answers (map_cl l e))
;;

(* let b = Apply(Lambda(Var("x"), Plus(Var("x"), Const(9))), Const(5));;
let clo = CLOSURE([],b)

let k = krivine clo [];; *)

