type bigint = sign * int list and sign = Neg|NonNeg;;
exception DivisionByZero;;
let failwith msg = raise (Failure msg);;

let rec conv_list integer lst = match integer with
	0 -> lst
|	_ -> conv_list (integer/10) ((integer mod 10)::lst)
;;

let conv_bigint simple_int = match simple_int with
	0 -> (NonNeg,[0])
|	_ when simple_int < 0 -> (Neg,conv_list (-1 * simple_int) [])
|	_ -> (NonNeg,conv_list simple_int [])
;;

let rec reduce_list lst = match lst with
	[] -> [0]
| 	0::tl -> reduce_list tl
|	_ -> lst
;;

let rec reduce (sgn,lst) = match reduce_list lst with 
	[]
|	[0] -> (NonNeg,[0])
|	_ as ls-> (sgn,ls)
;;



let string_of_bigint = function
	(NonNeg,lst) -> List.fold_left (fun str elem -> str^(string_of_int elem)) "" lst
|	(Neg,lst) -> List.fold_left (fun str elem -> str^(string_of_int elem)) "-" lst
;;

let neg = function
	(_,[0]) -> (NonNeg,[0])
|	(NonNeg,lst) -> (Neg,lst) 
|	(Neg,lst) -> (NonNeg,lst) 
;;

let abs = function
	(_,lst) -> (NonNeg,lst)
;;

let equal big1 big2 = match (reduce big1,reduce big2) with
	((s1,l1),(s2,l2)) -> ((s1=s2) && (l1 = l2))
;;

let less_than_list lst1 lst2 = 
	let new_lst1 = reduce_list(lst1) in
	let new_lst2 = reduce_list(lst2) in

	if List.length new_lst1 > List.length new_lst2 then
		false
	else if List.length new_lst1 < List.length new_lst2 then
		true
	else
		new_lst1 < new_lst2
;;
let less_or_equal_list lst1 lst2 = 
	let new_lst1 = reduce_list lst1 in
	let new_lst2 = reduce_list lst2 in
	if new_lst1 = new_lst2 then
		true
	else
		less_than_list new_lst1 new_lst2
;;

let less_than big1 big2 = if equal big1 big2 then false else match (reduce big1,reduce big2) with
	((NonNeg,l1),(Neg,l2)) -> false
|	((Neg,l1),(NonNeg,l2)) -> true
|	((NonNeg,l1),(NonNeg,l2)) -> less_than_list l1 l2
|	((Neg,l1),(Neg,l2)) -> less_than_list l2 l1
;;

let greater_than big1 big2 = less_than big2 big1
;;

let great_or_equal big1 big2 = if equal big1 big2 then true else greater_than big1 big2
;;

let less_or_equal big1 big2 = if equal big1 big2 then true else less_than big1 big2
;;


(* Assuming List.length lst1 > List.length lst2 *)
let rec add_temp lst1 carry new_list lst2 = match (lst1,lst2) with
		([],[]) -> reduce_list (carry::new_list)
	|	([],hd::tl) -> add_temp [] ((hd+carry)/10) (((hd+carry) mod 10) :: new_list) tl
	| 	(hd::tl,[]) -> add_temp tl ((hd+carry)/10) (((hd+carry) mod 10) :: new_list) []
	|	(hd1::tl1,hd2::tl2) -> add_temp tl1 ((hd1+hd2+carry)/10) (((hd1+hd2+carry) mod 10) :: new_list) tl2
;;

let add_list lst1 lst2 = add_temp (List.rev (reduce_list lst1)) 0 [] (List.rev(reduce_list lst2))
;;

(* Returns abs(lst1 - lst2) *)
let rec sub_temp lst1 carry new_list lst2 = match (lst1,lst2) with
	([],[]) -> if carry = 0 then new_list else add_list (List.map (fun x -> 9-x) new_list) [1]
|	([],hd::tl) -> sub_temp [] (1 - (10-hd-carry)/10) (((10-hd-carry) mod 10) :: new_list) tl
| 	(hd::tl,[]) -> sub_temp tl (1 - (10+hd-carry)/10) (((10+hd-carry) mod 10) :: new_list) []
|	(hd1::tl1,hd2::tl2) -> sub_temp tl1 (1 - (hd1+10-hd2-carry)/10) (((hd1+10-hd2-carry) mod 10) :: new_list) tl2
;;
let sub_list lst1 lst2 = sub_temp (List.rev (reduce_list lst1)) 0 [] (List.rev(reduce_list lst2))
;;

let rec add_big (big1: bigint) (big2: bigint) = reduce(
	match (big1,big2) with
	((NonNeg,l1),(NonNeg,l2)) -> (NonNeg,add_list l1 l2)
|	((Neg,l1),(Neg,l2)) -> (Neg,add_list l1 l2)
| 	((NonNeg,l1),(Neg,l2)) -> if less_than_list l1 l2 then (Neg, sub_list l2 l1) else (NonNeg, sub_list l1 l2)
|	((Neg,l1),(NonNeg,l2)) -> add_big big2 big1
);;

let sub_big (big1: bigint) (big2: bigint) = add_big big1 (neg big2)
;;



let rec mul_list_num_temp res carry num = function
	[] -> reduce_list (carry::res)
|	hd::tl -> (
			let new_elem = (hd * num + carry) mod 10 in
			let new_carr = (hd * num + carry) / 10 in
			mul_list_num_temp (new_elem::res) new_carr num tl
		)
;;

let mul_list_num lst1 num = 
	mul_list_num_temp [] 0 num (List.rev lst1)
;;

let rec mul_list res lst1 = function
	[] -> res 
|	hd::tl -> (
			let temp_res = mul_list_num res 10 in
			let temp_res2 = mul_list_num lst1 hd in
			mul_list (add_list temp_res temp_res2) lst1 tl
		)
;;

let mul_big (big1: bigint) (big2: bigint) =
	let (big1sn,big1lst) = big1 in
	let (big2sn,big2lst) = big2 in
	reduce(
		if big1sn = big2sn then
			(NonNeg,mul_list [] big1lst big2lst)
		else
			(Neg,mul_list [] big1lst big2lst)
	)
;;


let rec get_multiple num lst1 lst2 = 
	if less_or_equal_list (mul_list_num lst2 num) lst1 then
		num
	else
		get_multiple (num-1) lst1 lst2
;;


let rec div_list_temp lst1 lst2 = 
	if lst2 = [0] then raise DivisionByZero
	else if List.length lst1 < List.length lst2 then
		([0],lst1)
	else if List.length lst1 > List.length lst2 then
	(
		let (quo,dividend) = div_list_temp lst1 (mul_list_num lst2 10) in
		let mul = get_multiple 9 dividend lst2 in
		let rem = sub_list dividend (mul_list_num lst2 mul) in
		(mul::quo, reduce_list rem)
	)
	else
	(
		let mul = get_multiple 9 lst1 lst2 in
		let rem = sub_list lst1 (mul_list_num lst2 mul) in
		(mul::[],reduce_list rem )
	)
;;


let div_list lst1 lst2 =
	let rev2 = reduce_list lst2 in
	let rev1 = reduce_list lst1 in
	let (quo,rem) = div_list_temp rev1 rev2 in
	(reduce_list(List.rev quo),rem)
;;


let div_big_quo (big1: bigint) (big2: bigint) = 
	let (big1sn,big1lst) = big1 in
	let (big2sn,big2lst) = big2 in
	reduce(
		let (quo,rem) = div_list big1lst big2lst in
		if big1sn = big2sn then
			(NonNeg,quo)
		else
			(Neg,quo)
	)
;;


let div_big_rem (big1: bigint) (big2: bigint) = 
	let (big1sn,big1lst) = big1 in
	let (big2sn,big2lst) = big2 in
	reduce(
		let (quo,rem) = div_list big1lst big2lst in
		(big1sn,rem)
	)
;;

let print_big a = 
	print_string (string_of_bigint a)
;;

let rec fact acc = function
	(NonNeg,[1]) -> acc
|	a -> fact (mul_big acc a ) (sub_big a (NonNeg,[1]))
;;

let a = fact (NonNeg,[1]) (NonNeg,[1;0;0;0]);;
(* Printf.printf "%s\n" (string_of_bigint a);; *)
(* let b = fact (NonNeg,[1]) (NonNeg,[5;0;0]);; *)


(* 
let one = conv_bigint 1;;
let zero = conv_bigint 0;;
let pos = conv_bigint 1234;;
let nega = conv_bigint (-1*239);;

let ans1 = add_big pos pos;;
Printf.printf "%s + %s = %s\n" (string_of_bigint pos) (string_of_bigint pos) (string_of_bigint ans1);;
let ans2 = add_big pos nega;;
Printf.printf "%s + %s = %s\n" (string_of_bigint pos) (string_of_bigint nega) (string_of_bigint ans2);;
Printf.printf("\n");;

let ans3 = mul_big pos pos;;
let ans4 = mul_big pos nega;;
let ans5 = mul_big pos zero;;
let ans6 = mul_big pos one;;
Printf.printf "%s * %s = %s\n" (string_of_bigint pos) (string_of_bigint pos) (string_of_bigint ans3);;
Printf.printf "%s * %s = %s\n" (string_of_bigint pos) (string_of_bigint nega) (string_of_bigint ans4);;
Printf.printf "%s * %s = %s\n" (string_of_bigint pos) (string_of_bigint zero) (string_of_bigint ans5);;
Printf.printf "%s * %s = %s\n" (string_of_bigint pos) (string_of_bigint one) (string_of_bigint ans6);;
Printf.printf("\n");;

let ans7 = sub_big pos pos;;
let ans8 = sub_big pos nega;;
let ans9 = sub_big nega zero;;
Printf.printf "%s - %s = %s\n" (string_of_bigint pos) (string_of_bigint pos) (string_of_bigint ans7);;
Printf.printf "%s - %s = %s\n" (string_of_bigint pos) (string_of_bigint nega) (string_of_bigint ans8);;
Printf.printf "%s - %s = %s\n" (string_of_bigint nega) (string_of_bigint zero) (string_of_bigint ans9);;
Printf.printf("\n");;

let ans10 = div_big_quo pos pos;;
let ans11 = div_big_quo pos nega;;
let ans12 = div_big_quo pos one;;
(* let ans13 = div_big_quo pos zero;; *)
Printf.printf "%s / %s = %s\n" (string_of_bigint pos) (string_of_bigint pos) (string_of_bigint ans10);;
Printf.printf "%s / %s = %s\n" (string_of_bigint pos) (string_of_bigint nega) (string_of_bigint ans11);;
Printf.printf "%s / %s = %s\n" (string_of_bigint pos) (string_of_bigint one) (string_of_bigint ans12);;
(* Printf.printf "%s / %s = %s\n" (string_of_bigint pos) (string_of_bigint zero) (string_of_bigint ans13);; *)
Printf.printf("\n");;

let ans14 = div_big_rem pos pos;;
let ans15 = div_big_rem pos nega;;
let ans16 = div_big_rem pos one;;
(* let ans17 = div_big_rem pos zero;; *)
Printf.printf "%s %% %s = %s\n" (string_of_bigint pos) (string_of_bigint pos) (string_of_bigint ans14);;
Printf.printf "%s %% %s = %s\n" (string_of_bigint pos) (string_of_bigint nega) (string_of_bigint ans15);;
Printf.printf "%s %% %s = %s\n" (string_of_bigint pos) (string_of_bigint one) (string_of_bigint ans16);;
(* Printf.printf "%s % %s = %s\n" (string_of_bigint pos) (string_of_bigint zero) (string_of_bigint ans17);; *)
Printf.printf("\n");;

let ans17 = neg pos;;
let ans18 = neg nega;;
let ans19 = neg one;;
let ans20 = neg zero;;
Printf.printf "(neg)%s = %s\n" (string_of_bigint pos) (string_of_bigint ans17);;
Printf.printf "(neg)%s = %s\n" (string_of_bigint nega) (string_of_bigint ans18);;
Printf.printf "(neg)%s = %s\n" (string_of_bigint one) (string_of_bigint ans19);;
Printf.printf "(neg)%s = %s\n" (string_of_bigint zero) (string_of_bigint ans20);;
Printf.printf("\n");;

let ans21 = abs pos;;
let ans22 = abs nega;;
let ans23 = abs one;;
let ans24 = abs zero;;
Printf.printf "(abs)%s = %s\n" (string_of_bigint pos) (string_of_bigint ans21);;
Printf.printf "(abs)%s = %s\n" (string_of_bigint nega) (string_of_bigint ans22);;
Printf.printf "(abs)%s = %s\n" (string_of_bigint one) (string_of_bigint ans23);;
Printf.printf "(abs)%s = %s\n" (string_of_bigint zero) (string_of_bigint ans24);;
Printf.printf("\n");;


let ans25 = equal pos pos;;
let ans26= equal pos nega;;
let ans27 = equal pos zero;;
let ans28 = equal pos one;;
Printf.printf "%s == %s = %b\n" (string_of_bigint pos) (string_of_bigint pos) (ans25);;
Printf.printf "%s == %s = %b\n" (string_of_bigint pos) (string_of_bigint nega) (ans26);;
Printf.printf "%s == %s = %b\n" (string_of_bigint pos) (string_of_bigint zero) (ans27);;
Printf.printf "%s == %s = %b\n" (string_of_bigint pos) (string_of_bigint one) (ans28);;
Printf.printf("\n");;


let ans29 = greater_than pos pos;;
let ans30 = greater_than pos nega;;
let ans31 = greater_than pos zero;;
let ans32 = greater_than pos one;;
Printf.printf "%s > %s = %b\n" (string_of_bigint pos) (string_of_bigint pos) (ans29);;
Printf.printf "%s > %s = %b\n" (string_of_bigint pos) (string_of_bigint nega) (ans30);;
Printf.printf "%s > %s = %b\n" (string_of_bigint pos) (string_of_bigint zero) (ans31);;
Printf.printf "%s > %s = %b\n" (string_of_bigint pos) (string_of_bigint one) (ans32);;
Printf.printf("\n");;


let ans33 = less_than pos pos;;
let ans34 = less_than pos nega;;
let ans35 = less_than pos zero;;
let ans36 = less_than pos one;;
Printf.printf "%s < %s = %b\n" (string_of_bigint pos) (string_of_bigint pos) (ans33);;
Printf.printf "%s < %s = %b\n" (string_of_bigint pos) (string_of_bigint nega) (ans34);;
Printf.printf "%s < %s = %b\n" (string_of_bigint pos) (string_of_bigint zero) (ans35);;
Printf.printf "%s < %s = %b\n" (string_of_bigint pos) (string_of_bigint one) (ans36);;
Printf.printf("\n");;


let ans37 = great_or_equal pos pos;;
let ans38 = great_or_equal pos nega;;
let ans39 = great_or_equal pos zero;;
let ans40 = great_or_equal pos one;;
Printf.printf "%s >= %s = %b\n" (string_of_bigint pos) (string_of_bigint pos) (ans37);;
Printf.printf "%s >= %s = %b\n" (string_of_bigint pos) (string_of_bigint nega) (ans38);;
Printf.printf "%s >= %s = %b\n" (string_of_bigint pos) (string_of_bigint zero) (ans39);;
Printf.printf "%s >= %s = %b\n" (string_of_bigint pos) (string_of_bigint one) (ans40);;
Printf.printf("\n");;


let ans41 = less_or_equal pos pos;;
let ans42 = less_or_equal pos nega;;
let ans43 = less_or_equal pos zero;;
let ans44 = less_or_equal pos one;;
Printf.printf "%s <= %s = %b\n" (string_of_bigint pos) (string_of_bigint pos) (ans41);;
Printf.printf "%s <= %s = %b\n" (string_of_bigint pos) (string_of_bigint nega) (ans42);;
Printf.printf "%s <= %s = %b\n" (string_of_bigint pos) (string_of_bigint zero) (ans43);;
Printf.printf "%s <= %s = %b\n" (string_of_bigint pos) (string_of_bigint one) (ans44);;
Printf.printf("\n");; *)
