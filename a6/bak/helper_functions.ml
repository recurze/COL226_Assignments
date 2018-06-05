open Exceptions;;
open Types;;
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
