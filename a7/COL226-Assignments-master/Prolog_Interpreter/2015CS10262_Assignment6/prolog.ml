open Parser;;
open Printf;;
module SS = Set.Make(String);;

let load_rule fl = 
  let decl = open_in fl in
  let lexbuf = Lexing.from_channel decl in
  let prog = Parser.main Lexer.translate lexbuf in
  if (Structure.wfprog [] prog) then prog else prog
;;


let handle_query prog str = 
  let lexbuf = Lexing.from_string str in
  let goals = Parser.goal Lexer.translate lexbuf in
  let vars = List.fold_left (fun a b -> Structure.vars_atm a b) SS.empty goals in
  let ans = Structure.solve false vars (Structure.idty_subst) prog goals in match ans with
    Structure.False -> Printf.printf "false\n"
  | Structure.True sub -> Printf.printf "true\n"
;;

let rec main prog =
    Printf.printf "?- ";
    let inp = try read_line() with End_of_file -> Printf.printf "\nExiting\n" ;exit 0 in
    let () = flush stdout in            
    let len = String.length inp in
    if (inp.[(len-1)] <> '.') then
      let () = (Printf.printf "Every instruction must end with \'.\'\n") in
      main prog
    else
      if (inp = "halt.") then exit 0
      else
        if ((String.sub inp 0 2) = "[\"") && ((String.sub inp (len - 3) 3) = "\"].") then
          let name = String.sub inp 2 (len - 5) in 
          let new_prog = try let prog = (load_rule name) in let () = Printf.printf "Program loaded.\n" in prog with
            | Parsing.Parse_error -> let () = Printf.printf("Error in rule file given.\n") in prog 
            | Structure.NOT_WELLFORMED -> let () = Printf.printf("Rules in file are not well formed \n") in prog
            | _ -> let () = Printf.printf("Wrong file path provided.\n") in prog
          in main new_prog
        else
          let () = try (handle_query prog inp)
          with
          | Parsing.Parse_error -> Printf.printf "Invalid command\n"
          in main prog
;;


if !Sys.interactive then () else main [];;
    