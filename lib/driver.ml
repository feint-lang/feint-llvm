open Lexing
open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = UnitActionsParser.MenhirInterpreter

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

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> "???"

let succeed _v = assert false

let fail text buffer (checkpoint : _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  let message = ParserMessages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  eprintf "%s%s%s%!" location indication message;
  exit 1

let fallback file_name text =
  let lexbuf = L.init file_name (Lexing.from_string text) in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let parse_text text =
  let lexbuf = Lexing.from_string text in
  let ast = try_parse lexbuf text in
  ast

let parse_file file_name =
  let text, lexbuf = L.read file_name in
  let ast = try_parse lexbuf text in
  match ast with Some ast -> Some ast | None -> fallback file_name text
