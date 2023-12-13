
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx glb =
    print_string ">> ";
    flush stdout;
    try
      (* Added loop2 that reads lines until you find ;;, and when you do that, you read the last line without the ;; and finish *)
      let rec loop2 line =
        let a = read_line() in
        let n = String.length (line^" "^a) in
        if String.ends_with ~suffix:";;" a then (String.sub (line^" "^a) 0 (n - 2)) else loop2 (line^" "^a)
      in let tm = s token (from_string (loop2 String.empty)) in
      let str, globalv, gloCtx = eval tm glb ctx in 
      let tyTm = typeof gloCtx tm glb in
      print_endline (string_of_ty tyTm ^ " : " ^ string_of_term (str));
      loop gloCtx globalv
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx glb
     | Parse_error ->
         print_endline "syntax error";
         loop ctx glb
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx glb
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx emptyGlb
  ;;

top_level_loop ()
;;

