open Types;;
open Helper_functions;;
open Exceptions;;

let rec vars t =
    match t with
        V(x) -> [x]
    |   Const(x) -> []
    |   Cut -> []
    |   Node(_, t1) ->
            fold_left append [] (map vars t1)
;;

let rec wfterm t =
    match t with
        V(s) -> true
    |   Const(n) -> true
    |   Node(S(sym, arity), t1)->
            if arity <> (len t1) then
                false
            else
                fold_left andd true (map wfterm t1)
;;
(* new subst as a function *)
(* let rec subst sigma t = match t with
                        V(x) -> sigma(x)
                    |   Const(s) -> Const(s)
                    |   Node(sym, t1) -> Node(sym, map (subst sigma) t1)
;;
*)
(* changing subst back to list *)
let rec subst sigma t =
    match t with
        V(x) -> (
                    try (lookup x sigma)
                    with e -> V(x)
                )
    |   Const(s) -> Const(s)
    |   Node(sym, t1) ->
            Node(sym, map (subst sigma) t1)
;;

(* apply sigma2 then sigma 1 *)
(* let compose sigma1 sigma2 =
    fun x ->
        subst sigma1 (subst sigma2 (V(x)));;
*)
let compose sigma1 sigma2 =
    let rec aux s1 s2 i =
        match s1 with
            [] -> i
        |   (x, y)::ss -> aux ss s2 ((x, (subst s2 y))::i) in
    let ret = aux sigma1 sigma2 [] in
        let rec aux1 s i =
            match s with
                [] -> i
            |   (x, y)::ss ->
                    if ispresent x ss then aux1 ss i
                    else aux1 ss ((x,y)::i) in

    aux1 sigma2 ret
;;

let rec mgu term1 term2 =
    match (term1, term2) with
        (V(x), V(y)) ->
            if x=y then []
            else [(x, V(y))]
    |   (Const(x), Const(y)) ->
            if x=y then []
            else raise NotUnifiable
    |   (V(x), Const(y)) | (Const(y), V(x)) ->
            [(x, Const(y))]
    |   (V(x), Node(s, t1)) | (Node(s, t1), V(x))->
            let l = vars (Node(s, t1)) in
                if isInList x l then
                    raise NotUnifiable
                else
                    [(x, Node(s, t1))]
    |   (Node(S(s1, n1), t1), Node(S(s2, n2), t2)) ->
            if ((s1<>s2) || (n1 <> n2)) then
                raise NotUnifiable
            else mgu_list [] t1 t2
    |   _ -> raise NotUnifiable

(* let rec mgu term1 term2 =
    match (term1, term2) with
        (V(x), V(y)) ->
            fun v-> V(if x=v then y else v)
    |   (Const(x), Const(y)) ->
            if x=y then (fun v -> v)
            else raise NotUnifiable
    |   (V(x), Const(y)) | (Const(y), V(x)) ->
            fun v -> if x=v then Const(y) else V(v)
    |   (V(x), Node(s, t1)) | (Node(s, t1), V(x))->
            let l = vars (Node(s, t1)) in
                if isInList x l then
                    raise NotUnifiable
                else
                    fun v -> if v=x then Node(s, t1) else V(v)
    |   (Node(S(s1, n1), t1), Node(S(s2, n2), t2)) ->
            if ((s1<>s2) || (n1 <> n2)) then
                raise NotUnifiable
            else mgu_list [] t1 t2
*)
and mgu_list sigma l1 l2 =
    match (l1, l2) with
        ([], []) -> sigma
    |   (h::t, hh::tt) ->
            let sigma1 = mgu (subst sigma h) (subst sigma hh) in
                mgu_list (compose sigma sigma1) t tt
    |   _ -> raise NotUnifiable
;;
