(** The lexer takes an input stream and produces tokens. *)

{
open Lexing
open Parser

exception Error of string

(** Increment line number **)
let new_line lexbuf =
  Lexing.new_line lexbuf

(** Get current line number as string **)
let line_no lexbuf =
  let p = lexbuf.lex_curr_p in
  Format.sprintf "%d" p.pos_lnum

(** Get current column number as string **)
let col_no lexbuf =
  let p = lexbuf.lex_curr_p in
  Format.sprintf "%d" (p.pos_cnum - p.pos_bol + 1)

(** Format current line and column numbers **)
let format_pos lexbuf =
  Format.sprintf "line %s at column %s" (line_no lexbuf) (col_no lexbuf)

let keywords = Hashtbl.create 53
let _ = List.iter
  (fun (keyword, token) -> Hashtbl.add keywords keyword token)
  [
    ("nil", NIL); ("true", TRUE); ("false", FALSE);
    ("if", IF); ("else", ELSE); ("match", MATCH)
  ]
}

let ws = [' ' '\t']+
let nl = '\n' | "\r\n"

let ascii_lower = ['a' - 'z']
let ascii_lower_digits = ['a' - 'z' '0' - '9']
let ascii_lower_digits_underscore = ['a' - 'z' '0' - '9' '_']

let keyword = ascii_lower+

let placeholder_ident = '_'+
let ident = ascii_lower | ascii_lower ascii_lower_digits_underscore* ascii_lower_digits
let special_ident = '$' ident

let dec = ['0' - '9']
let bin = ['0' - '1']
let oct = ['0' - '7']
let hex = ['0' - '9' 'a' - 'f' 'A' - 'F']

let int_10 = '0' | ['1' - '9'] ('_'? dec+)*
let int_02 = "0b" bin+
let int_08 = "0o" oct+
let int_16 = "0x" hex+
let int = ['+' '-']? int_10 | int_02 | int_08 | int_16

let float =
    dec+ '.' dec+
  | dec+ ('.' dec+) ['e' 'E'] ['+' '-']? dec+

rule read = parse
  (* Whitespace *)
  | ws { read lexbuf }
  | nl ' '+ as indent { new_line lexbuf; INDENT(String.length(indent)) }
  | nl { new_line lexbuf; NL }
  (* Comments *)
  | '#' ws* { read_comment (Buffer.create 32) lexbuf }
  | "//" ws* { read_doc_comment (Buffer.create 32) lexbuf }
  (* Scopes *)
  | "->" { SCOPE_START }
  | "=>" { FUNC_START }
  (* Groupings *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACE }
  | ']' { RBRACE }
  | '{' { LBRACKET }
  | '}' { RBRACKET }
  (* Keywords *)
  | keyword as word {
      try Hashtbl.find keywords word
      with Not_found -> IDENT word
    }
  (* Types *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' { read_string (Buffer.create 32) lexbuf }
  (* Identifiers *)
  | ident as name { IDENT name }
  | special_ident as name { SPECIAL_IDENT name }
  (* Operators *)
  | '!' { BANG }
  | "!!" { BANG_BANG }
  | '^' { CARET }
  | '*' { STAR }
  | '/' { SLASH }
  | "//" { DOUBLE_SLASH }
  | '%' { PERCENT }
  | '+' { PLUS }
  | '-' { DASH }
  | '.' { DOT }
  (* Other *)
  | _ { raise (Error ("Unexpected character in input stream: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_comment buf = parse
  | nl ws* '#' ' '? {
      Buffer.add_char buf '\n';
      (* new_line lexbuf; *)
      read_comment buf lexbuf
    }
  | nl | eof {
      Buffer.add_char buf '\n';
      (* new_line lexbuf; *)
      COMMENT (Buffer.contents buf)
    }
  | _ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_comment buf lexbuf
    }

and read_doc_comment buf = parse
  | nl ws* "//" ' '? {
      Buffer.add_char buf '\n';
      new_line lexbuf;
      read_doc_comment buf lexbuf
    }
  | nl | eof {
      Buffer.add_char buf '\n';
      new_line lexbuf;
      DOC_COMMENT (Buffer.contents buf)
    }
  | _ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_doc_comment buf lexbuf
    }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error ("Illegal character in string: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error ("Unterminated string literal")) }
