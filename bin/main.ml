let parse text =
  let lexbuf = Lexing.from_string text in
  let ast = Feint.Parser.program Feint.Lexer.read lexbuf in
  ast

let text = ref ""
let arg_spec = [ ("-c", Arg.Set_string text, "Text") ]
let argv = ref []
let args arg = argv := arg :: !argv
let usage = "feint -c <text> [arg]"

let main () =
  Arg.parse arg_spec args usage;
  print_endline ("Parsing text: " ^ !text);
  parse !text
;;

main ()
