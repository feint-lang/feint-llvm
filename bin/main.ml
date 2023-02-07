open Printf

let text = ref ""
let arg_spec = [ ("-c", Arg.Set_string text, "Text") ]
let argv = ref []
let args arg = argv := arg :: !argv
let usage = "feint -c <text> [arg]"

let main () =
  Arg.parse arg_spec args usage;
  let result =
    if String.length !text > 0 then (
      printf "Parsing text\n";
      Feint.Driver.parse_text !text)
    else if List.length !argv > 0 then (
      printf "Parsing file: %s\n" (List.hd !argv);
      Feint.Driver.parse_file (List.hd !argv))
    else (
      eprintf "feint received neither a file name or a code snippet\n";
      exit (-1))
  in
  match result with
  | Some program -> Feint.Ast.display_ast program
  | None -> eprintf "\nAborted\n"
;;

main ()
