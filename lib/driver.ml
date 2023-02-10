open Lexing
open Printf

exception DriverErr of string

let try_parse lexbuf =
  match Parser.fmodule Lexer.read lexbuf with
  | ast -> Ok ast
  | exception LexerUtil.LexerErr msg ->
      let pos = LexerUtil.format_pos lexbuf in
      let msg = sprintf "Syntax error on %s:\n\n  %s\n" pos msg in
      prerr_endline msg;
      Error msg
  | exception Parser.Error ->
      let pos = LexerUtil.format_pos lexbuf in
      let msg = sprintf "Parser error on %s" pos in
      Error msg

(* Error handling --------------------------------------------------- *)

module E = MenhirLib.ErrorReports
module I = UnitActionsParser.MenhirInterpreter
module L = MenhirLib.LexerUtil

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
  | None -> raise (DriverErr "unreachable")

let succeed _fmodule =
  raise (DriverErr "unreachable (successfully parsed module while handling error)")

let fail text buffer checkpoint =
  let location = L.range (MenhirLib.ErrorReports.last buffer) in
  let indication = sprintf "Syntax error %s\n" (E.show (show text) buffer) in
  let message = ParserMessages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  prerr_endline (sprintf "%s%s%s%!" location indication message)

let handle_err file_name text =
  let lexbuf = L.init file_name (Lexing.from_string text) in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.fmodule lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

(* Entrypoints  ----------------------------------------------------- *)

let parse_text text =
  let lexbuf = Lexing.from_string text in
  try_parse lexbuf

let parse_file file_name =
  let text, lexbuf = L.read file_name in
  let result = try_parse lexbuf in
  match result with Ok fmodule -> Ok fmodule | Error msg -> Error (msg, text)
