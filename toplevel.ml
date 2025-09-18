(*----------*)
(* TOPLEVEL *)
(*----------*)

open Interpreter

let rec main () =
  print_string "> ";
  let input = read_line () in
  (match parse_expr (explode input) with
  | Some (expr, []) -> Printf.printf "= %s\n" (string_of_expr (eval expr))
  | Some (_, rest) ->
      Printf.printf "Erreur: Caractères superflus ('%s')\n" (implode rest)
  | _ -> print_endline "Erreur: Syntaxe invalide");
  main ()

let () = main ()
