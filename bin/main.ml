open Feint

let text = ref "__text_option_placeholder__"
let debug = ref false
let print_ast = ref false

let arg_spec =
  [
    ("-c", Arg.Set_string text, "Code snippet to run");
    ("-d", Arg.Set debug, "Enable debug mode (currently does nothing)");
    ("-p", Arg.Set print_ast, "Print AST");
  ]

let argv = ref []
let args arg = argv := arg :: !argv
let usage = "feint -c <text> -p [file_name_or_arg] ..."

let process_module fmodule =
  match !print_ast with
  | true -> Ast.display_statements fmodule
  | false -> (
      try
        let interpreter = new Interpreter.interpreter false in
        interpreter#interpret fmodule
      with Interpreter.InterpreterErr msg ->
        prerr_endline msg;
        exit 1)

let () =
  Arg.parse arg_spec args usage;
  if !text <> "__text_option_placeholder__" then
    match Driver.parse_text !text with
    | Ok fmodule -> process_module fmodule
    | _ -> Driver.handle_err "<text>" !text
  else if List.length !argv > 0 then
    let file_name = List.hd !argv in
    match Driver.parse_file file_name with
    | Ok fmodule -> process_module fmodule
    | Error (_, text) -> Driver.handle_err file_name text
  else
    let repl = new Interpreter.repl in
    repl#start
