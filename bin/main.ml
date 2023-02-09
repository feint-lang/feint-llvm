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

let main () =
  Arg.parse arg_spec args usage;
  let result =
    if String.length !text > 0 then Driver.parse_text !text
    else if List.length !argv > 0 then Driver.parse_file (List.hd !argv)
    else (
      eprintf "feint received neither a file name or a code snippet\n";
      exit (-1))
  in
  match result with
  | Some fmodule -> (
      match !print with
      | true -> Ast.display_statements fmodule
      | false -> Interpreter.interpret fmodule)
  | None -> eprintf "\nAborted\n"
;;

main ()
