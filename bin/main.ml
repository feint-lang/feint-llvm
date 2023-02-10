open Feint
open Printf

let text = ref "__text_option_placeholder__"
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

let display_module fmodule =
  match !print with
  | true -> Ast.display_statements fmodule
  | false -> Interpreter.interpret_statements fmodule false

let main () =
  Arg.parse arg_spec args usage;
  if !text <> "__text_option_placeholder__" then
    match Driver.parse_text !text with
    | Ok fmodule -> display_module fmodule
    | _ -> Driver.handle_err "<text>" !text
  else if List.length !argv > 0 then
    let file_name = List.hd !argv in
    match Driver.parse_file file_name with
    | Ok fmodule -> display_module fmodule
    | Error (_, text) -> Driver.handle_err file_name text
  else (
    printf "Feint REPL\n";
    Interpreter.main_loop true)
;;

main ()
