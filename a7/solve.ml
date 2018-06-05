(*
 * This file just has contents of all files
 * from A6. Issues while trying to make A7 run
 *)

exception NoSuchElement;;
exception NotUnifiable;;
exception Fail;;

type symbol = S of string * int;;

type term =  V of string
        |   Const of string
        |   Node of symbol * (term list)
        |   Cut
;;

type sigma = (string * term) list;;

type clauses = Fact of term | Rule of term * (term list);;
type program = clauses list;;
type goal = term list;;

(* Apply f to each element of l and return list *)
(* ret[i] = f(l[i]) *)
let map f l =
    let rec aux f l i = match l with
                    [] -> i
                |   x::y -> aux f y (i@[f x]) in
    aux f l []
;;

(* returns the length of the list *)
let len l =
    let rec aux l i = match l with
                    [] -> i
                |   x::y -> aux y (i+1) in
    aux l 0
;;

let rec fold_left f i l =
    match l with
        [] -> i
    |   x::y -> fold_left f (f(i, x)) y;;

let andd (a, b) = a && b;;
let append (a, b) = a @ b;;

let rec isInList e l =
    match l with
        [] -> false
    |   h::t -> if h=e then true
                else isInList e t
;;

let rec ispresent s si =
    match si with
        []->false
    |   (ss,a)::y->
            if ss=s then true
            else ispresent s y;;

let rec lookup x l =
    match l with
        [] -> raise NoSuchElement
    |   (a, b)::t ->
            if x=a then b
            else lookup x t

let rec print_answer si=
    let rec aux a =
        match a with
            [] -> Printf.printf "\n\n";
        |   (x, V(y))::ss ->Printf.printf "%s = %s " x y; aux ss
        |   (x, Const(y))::ss -> Printf.printf "%s = %s " x y; aux ss in
    map aux si
;;

let clean a l =
    let rec aux l a = match a with
        [] -> a
    |   (x, V(y))::aa ->
            if not (isInList x l) then aux l aa
            else (x, V(y))::(aux l aa)
    |   (x, Const(y))::aa->
            if not (isInList x l) then aux l aa
            else (x, Const(y))::(aux l aa) in
    map (aux l) a
;;

let remove_dups a =
    let rec aux a i =
        match a with
            [] -> i
        |   x::aa ->
                if isInList x i then aux aa i
                else aux aa (x::i) in
    aux a []
;;

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
                    |   Node(sym, t1) ->
                            Node(sym, map (subst sigma) t1)
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
        |   (x, y)::ss ->
                aux ss s2 ((x, (subst s2 y))::i) in
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
                    fun v -> if v=x then Node(s, t1)
                             else V(v)
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
(*
 * s-> stack
 * cp-> current_program
 * fp-> full_program
 * si-> sigma
*)

let answer : (((string * term) list) list) ref = ref [[]];;

let rec eval_util (s, cp, fp, si, goal) =
    match (s, cp, goal) with
        ([], _, []) -> answer := si::(!answer)
    |   ((g, p, si1)::ss, _,  []) ->
            answer := si::(!answer);
            eval_util(ss, p, fp, si1, g)

    |   ([], [], g) -> raise Fail
    |   ((g, p, si1)::ss, [],  _)->
            eval_util (ss, p, fp, si1, g)

    |   (s, Fact(t)::p, g::grest) ->(
            try (
                let si1 = compose si (mgu t (subst si g)) in
                    eval_util (((g::grest, p, si)::s), fp, fp, si1, grest)
            )
            with _ -> eval_util (s, p, fp, si, (g::grest))
        )
    |   (s, (Rule(h, b))::p, g::grest) ->
            try(
                let si1 = compose si (mgu h (subst si g)) in
                    eval_util (((g::grest, p, si)::s), fp, fp, si1, grest@b)
            )
            with _-> eval_util (s, p, fp, si, (g::grest))
;;

let eval p g =
    try
        eval_util ([], p, p, [], g)
    with e->
        if !answer = [[]] then Printf.printf "Fail"
        else Printf.printf "True"
;;
let print g =
    let a = clean (!answer) (fold_left append [] (map vars g)) in
        let b = remove_dups a in
            answer:= [[]];
            print_answer b
;;

let solve p g =  eval p g; print g;;
