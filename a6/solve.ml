let rec print v sigma =
    match v with
        []-> Printf.printf ""
    |   V(h)::t ->
            let x = (subst sigma (V h)) in
                match x with Const(s) ->
                    Printf.printf "%s = %s\n" h s; print t sigma
;;
let rec solve program goal sigma v =
    if program = [] then Printf.printf "fail"
    else if goal = [] then print v sigma
    else begin
    let rec aux p (g::gs) si full v =
        match p with
            []-> Printf.printf "fail";
        |   Fact(h)::t -> (
                try(
                    let si1 = mgu h (subst si g) in
                    let a = solve full gs (compose si si1) v in
                    aux t (g::gs) si program v
                )
                with _ -> aux t (g::gs) si program v
            )
        |   Rule(h, b)::t -> (
                try(
                    let si1 = mgu h (subst si g) in
                    let a =solve full (b@gs) (compose si si1) v in
                    aux t (g::gs) si program v
                )
            with _ -> aux t (g::gs) si  program v
            )
    in
    aux program goal sigma program v
    end
;;
let go program goal =
    let v = fold_left append [] (map vars goal) in
        solve program goal (fun x-> V(x)) v
