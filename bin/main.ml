open Lexing
open Printf

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let try_parse lexbuf =
  try Feint.Parser.program Feint.Lexer.read lexbuf
  with Feint.Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)

let parse text =
  let lexbuf = Lexing.from_string text in
  let ast = try_parse lexbuf in
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
