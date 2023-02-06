{
  (* open Lexing *)
  open Parser

  exception SyntaxError of string
}

let ws = [' ' '\t']+
let nl = '\n' | "\r\n"

let int_10 = '0' | ['1' - '9'] ('_'? ['0' - '9']+)*
let int_02 = '0' 'b' ['0' - '1']+
let int_08 = '0' 'o' ['0' - '7']+
let int_16 = '0' 'x' ['0' - '9' 'a' - 'f' 'A' - 'F']+
let int = ['+' '-']? int_10 | int_02 | int_08 | int_16

rule read =
  parse
  |  ws { read lexbuf }
  | "nil" { NIL }
  | "true" { TRUE }
  | "false" { FALSE }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' { read_string (Buffer.create 32) lexbuf }
  | nl { NL }
  | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_string buf =
  parse
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
