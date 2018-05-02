let rec lookup x l =
    match l with
        [] -> raise NoSuchElement
    |   (a, b)::t ->
            if x=a then b
            else lookup x t
;;

let rec isInList s l =
    match l with
        []->false
    |   x::y->
            if x=s then true
            else isInList s y
;;

let rec ispresent s si =
    match si with
        []->false
    |   (ss,a)::y->
            if ss=s then true
            else ispresent s y
;;

let rec map f l initial = match l with
                []-> initial
            |   x::y-> map f y (initial@[f x]);;
(*
let rec reduce f l initial = match l with
                []->initial
            |   x::y-> reduce  f y (f x initial);;
 *)
let rec len li l = match li with
                    []->l
                |   x::y-> len y (l+1);;

let rec at m s = match m with
                    []->raise NoSuchElement
                |   (ss,a)::y-> if ss=s then a
                                else at y s;;

let sum a b = a+b;;
let andd a b = a && b;;
let orr a b = a || b;;
let append a b = a@b;;
let iszero (a,b) = (b=0);;
