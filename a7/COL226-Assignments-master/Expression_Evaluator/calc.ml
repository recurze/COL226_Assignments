(* File calc.ml *)
open Typefile;;

let _ =
    try
    let lexbuf = Lexing.from_channel stdin in
    while true do
        let result = Parser.main Lexer.translate lexbuf in
        let () = Printf.printf "%s\n" (string_of_parse_tree result) in
        let hash_map = make_kai [] result in
        Printf.printf "Result => %s\n" (string_of_mix ( eval_parse_tree hash_map result));
        flush stdout
    done
    with Lexer.Eof ->
    exit 0