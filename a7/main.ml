open Parser;;
open Solve;;
exception NoInputFile;;

let read_file f =
        try(
            let buffer = Lexing.from_channel (open_in f) in
            Parser.program Lexer.read buffer
        )
        with _ ->
            raise NoInputFile
;;

let print p =
    let rec aux f =
        match f with
        Solve.Fact(Node(S(s, n), t)) -> Printf.printf "%s" s
    |   Solve.Rule(Node(S(s, n), t), t1) -> Printf.printf "%s" s
    in
    Solve.map aux p;;
let query p q =
    let buffer = Lexing.from_string q in
        let goals =
            try Parser.goal Lexer.read buffer
            with _ ->
                Printf.printf "
1. Wrap non-variable and non-constructor strings in double quotes
2. End with a period(\'.\')";
                []
            in
            Solve.solve p goals
;;

let rec main p=
    Printf.printf ">>> ";
    let q =
        try read_line()
        with _ ->
            Printf.printf "Bye :)\n";
            exit 0
    in
        if q="" then
            main p
        else
        if (String.get q 0) = Char.uppercase (String.get q 0) then
        begin
            Printf.printf "42\n";
            main p
        end
        else if q="halt." then exit 0
        else begin
            query p q;
            main p
        end
;;
let runforever =
    let file_name =
        try Sys.argv.(1)
        with _ ->
            Printf.printf "
Enter input file as command line argument\n";
            ""
    in
    if file_name <> "" then
        let program =
            try read_file file_name
            with _ ->
                Printf.printf "
No such file or something wrong with it, idk\n";
                []
        in
        if program <> [] then
            let a = print program in
            main program
;;

