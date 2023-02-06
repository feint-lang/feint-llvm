{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

let ws = [' ' '\t']+
let nl = '\n' | "\r\n"

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
  | nl ' '+ as indent { next_line lexbuf; INDENT(String.length(indent)) }
  | nl { next_line lexbuf; NL }
  (* Comments *)
  | "//" { read_doc_comment lexbuf }
  | '#' { read_throwaway_comment lexbuf }
  (* Groupings *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACE }
  | ']' { RBRACE }
  | '{' { LBRACKET }
  | '}' { RBRACKET }
  (* Keywords *)
  | "nil" { NIL }
  | "true" { TRUE }
  | "false" { FALSE }
  (* Types *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' { read_string (Buffer.create 32) lexbuf }
  (* Operators *)
  | '*' { STAR }
  | '/' { SLASH }
  | "//" { DOUBLE_SLASH }
  | '+' { PLUS }
  | '-' { DASH }
  (* Other *)
  | _ { raise (SyntaxError ("Unexpected character in input stream: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_doc_comment = parse
  | nl ws* "//" { next_line lexbuf; read_doc_comment lexbuf }
  | nl { next_line lexbuf; read lexbuf }
  | _ { read_doc_comment lexbuf }
  | eof { EOF }

and read_throwaway_comment = parse
  | nl { next_line lexbuf; read lexbuf }
  | _ { read_throwaway_comment lexbuf }
  | eof { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal character in string: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Unterminated string literal")) }
