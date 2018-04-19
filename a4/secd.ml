let addd (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Int(aa1+aa2);;
let subb (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Int(aa1-aa2);;
let divv (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Int(aa1/aa2);;
let mull (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Int(aa1*aa2);;
let modd (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Int(aa1 mod aa2);;
let abss a1 = match a1 with Int aa1->if aa1<0 then Int (-aa1) else Int aa1;;
let nott a1 = match a1 with Bool t->Bool(not t);;
let andd (a1, a2) = match (a1, a2) with (Bool aa1, Bool aa2)->Bool(aa1 && aa2);;
let orrr (a1, a2) = match (a1, a2) with (Bool aa1, Bool aa2)->Bool(aa1 || aa2);;
let implyy (a1, a2) = match (a1, a2) with (Bool aa1, Bool aa2)-> Bool(not aa1 || aa2);;
let greatt (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)-> Bool(aa1>aa2);;
let equall (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Bool(aa1=aa2);;
let lessss (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Bool(aa1<aa2);;
let grequu (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Bool(aa1>=aa2);;
let leequu (a1, a2) = match (a1, a2) with (Int aa1, Int aa2)->Bool(aa1<=aa2);;
let expo (a1, a2) = match (a1, a2) with (Int aa1, Int aa2) -> Int(poww aa1 aa2);;

let rec compile e = match e with  
                    True->              [TRUE]
                |   False->             [FALSE]
                |   Const n->           [CONST n]
                |   Var s->             [VAR s]
                |   Abs e1->            (compile e1)@[ABS]
                |   Not e1->            (compile e1)@[NOT]
                |   List e->            [LIST (map compile e)]
                |   Plus (e1,e2)->      (compile e1)@(compile e2)@[PLUS]
                |   Minus (e1,e2)->     (compile e1)@(compile e2)@[MINUS]
                |   Mul (e1,e2)->       (compile e1)@(compile e2)@[MUL]
                |   Div (e1,e2)->       (compile e1)@(compile e2)@[DIV]
                |   Mod (e1,e2)->       (compile e1)@(compile e2)@[MOD]
                |   Pow (e1,e2)->       (compile e1)@(compile e2)@[POW]
                |   And (e1,e2)->       (compile e1)@(compile e2)@[AND]
                |   Orr (e1,e2)->       (compile e1)@(compile e2)@[ORR]
                |   Imply (e1,e2)->     (compile e1)@(compile e2)@[IMPLY]
                |   Equal (e1,e2)->     (compile e1)@(compile e2)@[EQUAL]
                |   Great (e1,e2)->     (compile e1)@(compile e2)@[GREAT]
                |   Lesss (e1,e2)->     (compile e1)@(compile e2)@[LESSS]
                |   Grequ (e1,e2)->     (compile e1)@(compile e2)@[GREQU]
                |   Leequ (e1,e2)->     (compile e1)@(compile e2)@[LEEQU]
                |   Projj (e1,List e2)->(compile (ith(e1,e2)))
                (* New additons *)
                |   LetInEnd(Var x, e1, e2) -> (compile e1)@[BIND(VAR x)]@(compile e2)@[UNBIND(VAR x)]
                |   Lambda(Var(x), e1)-> [CLOS(VAR(x), compile(e1)@[RET])]
                |   Apply(e1, e2)  -> compile(e1) @ compile(e2) @ [APP]
                |   Ite(c, et, ef) -> compile(c) @ [COND(compile(et), compile(ef))]
;;

let rec get_var x e = match e with
                      [] -> raise Undeclared_Variable
                    | (v,a)::t -> if v=x then a else get_var x t
;;

let rec map_ex f ex env= match ex with 
                        [] -> []
                    |   x::y -> (f [] env x [])::(map_ex f y env)
;;

let rec execute s e c d = match (s, c, d) with
                            ([x], [], d) -> x
                        |   (s, CONST(n)::cc, d) -> execute ((Int n)::s) e cc d
                        |   (s, TRUE::cc, d) -> execute ((Bool true)::s) e cc d
                        |   (s, FALSE::cc, d) -> execute ((Bool false)::s) e cc d
                        |   (s, VAR(x)::cc, d) -> execute ((get_var (Var x) e)::s) e cc d
                        |   (n::s, ABS::cc, d) -> execute ((abss n)::s) e cc d
                        |   (n::s, NOT::cc, d) -> execute ((nott n)::s) e cc d
                        |   (n::m::s, PLUS::cc, d)-> execute (addd(n,m)::s) e cc d
                        |   (n::m::s, MUL::cc, d)-> execute (mull(n,m)::s) e cc d
                        |   (n::m::s, MINUS::cc, d)-> execute (subb(m,n)::s) e cc d
                        |   (n::m::s, DIV::cc, d)-> execute (divv(m,n)::s) e cc d
                        |   (n::m::s, MOD::cc, d)-> execute (modd(m,n)::s) e cc d
                        |   (n::m::s, POW::cc, d)-> execute (expo(m,n)::s) e cc d
                        |   (n::m::s, AND::cc, d)-> execute (andd(n,m)::s) e cc d
                        |   (n::m::s, ORR::cc, d)-> execute (orrr(n,m)::s) e cc d
                        |   (n::m::s, IMPLY::cc, d)-> execute (implyy(m,n)::s) e cc d
                        |   (n::m::s, EQUAL::cc, d)-> execute (equall(n,m)::s) e cc d
                        |   (n::m::s, GREAT::cc, d)-> execute (greatt(m,n)::s) e cc d
                        |   (n::m::s, LESSS::cc, d)-> execute (lessss(m,n)::s) e cc d
                        |   (n::m::s, GREQU::cc, d)-> execute (grequu(m,n)::s) e cc d
                        |   (n::m::s, LEEQU::cc, d)-> execute (leequu(m,n)::s) e cc d
                        |   (s, (LIST ex)::cc, d)->execute ((Listans (map_ex execute ex e))::s) e cc d
                        (* New additions *)
                        |   (a::s, BIND(VAR(x))::cc, d)-> execute s ((Var(x), a)::e) cc d
                        |   (s, UNBIND(VAR(x))::cc, d)-> execute s (remove (Var x) e) cc d
                        |   (s, CLOS(y,op)::cc, d) ->
                            execute (VCL(e, y, op)::s) e cc d

                        |   (a::VCL(ee, VAR(y), op)::ss, APP::cc, d) ->
                            execute [] ((Var(y), a)::ee) op ((ss, e, cc)::d)

                        |   ([a], RET::cc, (sd,ed,cd)::dd) ->
                            execute (a::sd) ed cd dd

                        |   (Bool(true)::ss, COND(et,ef)::cc, d) ->
                            execute ss e (et@cc) d

                        |   (Bool(false)::ss, COND(et,ef)::cc, d) ->
                            execute ss e (ef@cc) d
;;
