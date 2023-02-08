open Lexing
open Parser

exception Error of string

let raise_err msg = raise (Error msg)
let curr_pos lexbuf = lexbuf.lex_start_p

(** Increment line number *)
let new_line lexbuf = Lexing.new_line lexbuf

(** Increment line number N times *)
let rec new_lines lexbuf count =
  match count with
  | 0 -> ()
  | _ ->
      new_line lexbuf;
      new_lines lexbuf (count - 1)

(** Get current line number as string *)
let line_no lexbuf =
  let p = lexbuf.lex_curr_p in
  Format.sprintf "%d" p.pos_lnum

(** Get current column number as string *)
let col_no lexbuf =
  let p = lexbuf.lex_curr_p in
  Format.sprintf "%d" (p.pos_cnum - p.pos_bol + 1)

(** Format current line and column numbers *)
let format_pos lexbuf =
  Format.sprintf "line %s at column %s" (line_no lexbuf) (col_no lexbuf)

(** Keywords *)
let keywords = Hashtbl.create 7

let _ =
  List.iter
    (fun (keyword, token) -> Hashtbl.add keywords keyword token)
    [
      ("nil", NIL);
      ("true", TRUE);
      ("false", FALSE);
      ("block", BLOCK);
      ("if", IF);
      ("else", ELSE);
      ("match", MATCH);
    ]

let get_keyword word =
  try Hashtbl.find keywords word with Not_found -> IDENT word

(** Count newlines in string *)
let count_newlines str =
  let count = ref 0 in
  String.iter (fun c -> if c = '\n' then incr count) str;
  !count

(** Decrement column number

    Used when lexing strings to set the start position at the opening
    quote.
*)
let dec_col_no lexbuf count =
  let p = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { p with pos_cnum = p.pos_cnum - count }

let process_str str =
  let str_len = String.length str in
  let peek_str = String.sub str 1 (str_len - 1) ^ " " in
  let buf = Buffer.create (String.length str) in
  let add = Buffer.add_char buf in
  let skip = ref false in
  List.iter2
    (fun c d ->
      match (c, d) with
      (* handle escaped char *)
      | '\\', _ -> (
          skip := true;
          match d with
          | '\n' -> ()
          | '\\' -> add '\\' (* backslash *)
          | '0' -> add (Char.chr 0) (* null *)
          | 'a' -> add (Char.chr 7) (* bell *)
          | 'b' -> add (Char.chr 8) (* backspace *)
          | 'f' -> add (Char.chr 12) (* form feed *)
          | 'n' -> add '\n' (* newline *)
          | 'r' -> add '\r' (* carriage return *)
          | 't' -> add '\t' (* tab *)
          | 'v' -> add (Char.chr 11) (* vertical tab *)
          (* unescape escaped quotes *)
          | '"' -> add '"'
          | '\'' -> add '\''
          (* other escaped chars resolve to the original escape sequence *)
          | _ ->
              add '\\';
              add d)
      (* handle unescaped char *)
      | _ -> if !skip then skip := false else add c)
    (List.of_seq (String.to_seq str))
    (List.of_seq (String.to_seq peek_str));
  Buffer.contents buf
