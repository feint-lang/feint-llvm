module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

let try_parse lexbuf =
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      let pos = Lexer.format_pos lexbuf in
      Printf.eprintf "Syntax error on %s:\n\n  %s\n" pos msg;
      exit (-1)
  | Parser.Error ->
      let pos = Lexer.format_pos lexbuf in
      Printf.eprintf "Parse error on %s\n" pos;
      exit (-1)

(** Parse text

    On success, do nothing. On error, show an error message. *)
let parse_text text =
  let lexbuf = Lexing.from_string text in
  let ast = try_parse lexbuf in
  ast

(** Parse file contents

    On success, do nothing. On error, show an error message. *)
let parse_file file_name =
  In_channel.with_open_text file_name (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      let ast = try_parse lexbuf in
      ast)
