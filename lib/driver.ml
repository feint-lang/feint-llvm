module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

let try_parse lexbuf text =
  match Parser.program Lexer.read lexbuf with
  | ast -> Some ast
  | exception Lexer.Error msg ->
      let pos = Lexer.format_pos lexbuf in
      Printf.eprintf "Syntax error on %s:\n\n  %s\n" pos msg;
      Printf.eprintf "\n--------------------\n%s--------------------\n" text;
      None
  | exception Parser.Error ->
      let pos = Lexer.format_pos lexbuf in
      Printf.eprintf "Parse error on %s\n" pos;
      Printf.eprintf "\n--------------------\n%s--------------------\n" text;
      None

(** Parse text

    On success, do nothing. On error, show an error message. *)
let parse_text text =
  let lexbuf = Lexing.from_string text in
  let ast = try_parse lexbuf text in
  ast

(** Parse file contents

    On success, do nothing. On error, show an error message. *)
let parse_file file_name =
  let text, lexbuf = L.read file_name in
  let ast = try_parse lexbuf text in
  ast
