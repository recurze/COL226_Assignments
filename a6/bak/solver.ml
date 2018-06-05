open Types;;
open Exceptions;;
open Mgu;;
open Helper_functions;;

(*
 * s-> stack
 * cp-> current_program
 * fp-> full_program
 * si-> sigma
*)

let answer : (((string * term) list) list) ref = ref [[]];;

let rec eval_util (s, cp, fp, si, goal) =
    match (s, cp, goal) with
        ([], _, []) -> (*print_sigma si v;*) answer := si::(!answer)
    |   ((g, p, si1)::ss, _,  []) ->
            (*print_sigma si v;*) answer := si::(!answer);
            eval_util(ss, p, fp, si1, g)

    |   ([], [], g) -> raise Fail
    |   ((g, p1, si1)::ss, p, Cut::grest) ->(
            try eval_util([], p, fp, si, grest)
            with _ -> eval_util(ss, p1, fp, si1, g)
        )
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
                    eval_util (((g::grest, p, si)::s), fp, fp, si1, b)
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
        let a = clean (!answer) (fold_left append [] (map vars g))  in
            let b = remove_dups a in
                print_answer b
;;
let solve p g = eval p g; print g;;
