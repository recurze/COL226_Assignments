exception Empty of string;;
exception AtFirst of string;;
exception AtLast of string;;
exception TooShort of string;;

type letter = char;;
(* type letter = alphabet;; *)
type astar = {marker:int ref; strlist: letter array; length:int ref };;

(*
    The comments after the function define the signature of that function. 
    And additional comments required by the assignment 
    for time complexity and proofs.
*)

let lgh s = !(s.length);;
(* val lgh : astar -> int = <fun>  *)

let nonempty s = (!(s.length)!=0) ;;
(* val nonempty : astar -> bool = <fun>  *)

let concat a b = {marker=ref 0; 
                  strlist=Array.append a.strlist b.strlist;
                  length=ref (!(a.length)+ (!(b.length)))
                 };;
(* val concat : astar -> astar -> astar = <fun>  *)
(*
    Time complexity of (concat a b) = O(n+m) where n, m are the lengths
    of a, b resp.
    lgh (concat a b) = (lgh a) + (lgh b)
    I defined len as n+m, as extending a by b results in a list of n+m elements.
 *)

let reverse s =
    let a = Array.copy s.strlist in
    let n = !(s.length) in
    for i=0 to n-1 do
        a.(i) <- (s.strlist).(n-1-i) done;

    {marker=ref 0; strlist=a; length=s.length};;
(* val reverse : astar -> astar = <fun> *)
(*
    len (reverse s) = len s
    Once again, len of reverse s is defined as len of s, as no elements are
    added or removed from the charlist.
 *)

let first s =
    if (!(s.length)!=0) then s.strlist.(0)
    else raise (Empty "The string is empty");;
(* val first : astar -> letter = <fun>  *)

let last s =
    if (!(s.length)!=0) then s.strlist.(!(s.length)-1)
    else raise (Empty "The string is empty");;
(* val last : astar -> letter = <fun> *)

let create s =
    if s="" then 
        {marker=ref 0; strlist = [||]; length=ref 0}
    else begin
        let n = String.length s in
        let a = Array.make n s.[0] in
        for i = 1 to (n-1) do 
            a.(i) <- s.[i] 
        done;
        {marker=ref 0; strlist=a; length=ref n}
    end;;
(* val create : string -> astar = <fun>  *)

let forward s =
    if !(s.marker) != !(s.length)-1 then s.marker:=!(s.marker)+1
    else raise (AtLast "Reached the end of the string");;
(* val forward : astar -> unit = <fun> *)
(* 
    Time complexity of this function is = O(1), finite expressions to be evaled.
*)

let back s =
    if !(s.marker) != 0 then s.marker:=!(s.marker)-1
    else raise (AtFirst "Reached the start of the string");;
(* val back : astar -> unit = <fun> *)
(* Time complexity of this function is O(1), reason mentioned above *)

let moveTo n s =
    if n < !(s.length) then s.marker:=n
    else raise (TooShort "The string doesn't contain so many characters");;
(* val moveTo : int -> astar -> unit = <fun> *)
(*
    Time complexity of this function is O(1) better than required O(n).
    O(n) algorithm for this function is to call forward at most n times.
    (or backward depending on the current position of marker) 
*)

let replace w s = s.strlist.(!(s.marker)) <- w;;
(* val replace : letter -> astar -> astar = <fun> *) 
(*
    len (replace a b) = len b
    One element of the list is being replaced by another so effectively,
    no elements have been added or removed. So the number of elements and so the
    length remains same.
*)
