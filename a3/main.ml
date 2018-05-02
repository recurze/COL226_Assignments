open Types;;
open Exceptions;;
open Helper_functions;;

let a_signature=[(S("x"),0); (S("y"),0); (S("+"),2); (S("-"),2); (S("fuckup"),-1); (S("rand"),3)];;
let b_signature=[(S("x"),0); (S("y"),0); (S("+"),2); (S("-"),2); (S("rand"),3)];;

let check_sig si =
    let rec aux si ret=
        match si with
            []-> ret
        |   (s,a)::y->
                if a<0 then false
                else if ispresent s y then
                    false
                else if a=0 then
                     aux y true
                else aux y ret in
    aux si false;;

let rec wfterm si t =
    match t with
        V(Var(_))-> true
    |   Node(s,t1)->
        if ((at si s) <> (len t1 0)) then
            false
        else
            (List.fold_left andd true (map (wfterm si) t1 []))
;;

let rec ht t =
    match t with
        V(Var(_))-> 0
    |   Node(s,t1)->
            1+List.fold_left max 0 (map ht t1 []);;

let rec size t =
    match t with
        V(Var(_))-> 1
    |   Node(s,t1)->
            1+List.fold_left sum 0 (map size t1 [])
;;


let rec union y x =
    match y with
        []->x
    |   h::t->
            if present_list h x then
                union t x
            else union t (h::x);;

let rec vars t =
    match t with
        V(x)-> [x]
    |   Node(s,t1)->
            List.fold_left union [] (map vars t1 []);;

let rec subst sigma t =
    match t with
        V(x) -> (
            try (lookup x sigma)
            with e -> V(x)
        )
    |   Node(sym, t1) ->
            Node(sym, map (subst sigma) t1)
;;

let compose sigma1 sigma2 =
    let rec aux s1 s2 i =
        match s1 with
            [] -> i
        |   (x, y)::ss ->
                aux ss s2 ((x, (subst s2 y))::i)
    in
    let ret = aux sigma1 sigma2 [] in
        let rec aux1 s i =
            match s with
                [] -> i
            |   (x, y)::ss ->
                    if ispresent x ss then
                        aux1 ss i
                    else
                        aux1 ss ((x,y)::i)
    in
    aux1 sigma2 ret
;;

let rec mgu term1 term2 =
    match (term1, term2) with
        (V(x), V(y)) ->
            if x=y then []
            else [(x, V(y))]
    |   (V(x), Node(s, t1)) | (Node(s, t1), V(x))->
            let l = vars (Node(s, t1)) in
                if isInList x l then
                    raise NotUnifiable
                else
                    [(x, Node(s, t1))]
    |   (Node(s1, t1), Node(s2, t2)) ->
            if s1<>s2 then
                raise NotUnifiable
            else mgu_list [] t1 t2
    |   _ -> raise NotUnifiable

and mgu_list sigma l1 l2 =
    match (l1, l2) with
        ([], []) -> sigma
    |   (h::t, hh::tt) ->
            let sigma1 =
                mgu (subst sigma h) (subst sigma hh)
            in
            mgu_list (compose sigma sigma1) t tt
    |   _ -> raise NotUnifiable
;;
