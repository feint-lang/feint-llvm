open Feint
open Printf

let text = ref ""
let debug = ref false
let print = ref false

let arg_spec =
  [
    ("-c", Arg.Set_string text, "Code snippet to run");
    ("-d", Arg.Set debug, "Enable debug mode (currently does nothing)");
    ("-p", Arg.Set print, "Print AST");
  ]

let argv = ref []
let args arg = argv := arg :: !argv
let usage = "feint -c <text> -p [file_name_or_arg] ..."

let display_module = function
  | Ok fmodule -> (
      match !print with
      | true -> Ast.display_statements fmodule
      | false -> Interpreter.interpret fmodule)
  | _ -> eprintf "\nAborted\n"

let main () =
  Arg.parse arg_spec args usage;
  if String.length !text > 0 then display_module (Driver.parse_text !text)
  else if List.length !argv > 0 then display_module (Driver.parse_file (List.hd !argv))
  else printf "Feint REPL\n";
  Interpreter.main_loop ()
;;

main ()
