{
open LexerUtil
open Parser
open Printf
}

let ws = [' ' '\t']+
let nl = '\n' | "\r\n"

let ascii_lower = ['a' - 'z']
let ascii_lower_digits = ['a' - 'z' '0' - '9']
let ascii_lower_digits_underscore = ['a' - 'z' '0' - '9' '_']

let keyword = '$'? ascii_lower+

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
  | ('#' ([^ '\n']* as comment) ('\n' | eof)) {
      new_line lexbuf;
      COMMENT (String.trim comment)
    }
  | ("//" ([^ '\n']* as comment) ('\n' | eof)) {
      new_line lexbuf;
      DOC_COMMENT (String.trim comment)
    }
  (* Scopes *)
  | "->" ws* nl { new_line lexbuf; SCOPE_START }
  | "->" { INLINE_SCOPE_START }
  | "=>" ws* nl { new_line lexbuf; FUNC_START }
  | "=>" { INLINE_FUNC_START }
  (* Groupings *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACE }
  | ']' { RBRACE }
  | '{' { LBRACKET }
  | '}' { RBRACKET }
  (* Keywords *)
  | keyword as word { get_keyword word }
  (* Types *)
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '"' {
      dec_col_no lexbuf 1;
      try
        read_double_quoted_string lexbuf
      with Failure msg ->
        raise_err (sprintf "Unterminated string literal (%s)" msg)
    }
  | '\'' {
      dec_col_no lexbuf 1;
      try
        read_single_quoted_string lexbuf
      with Failure msg ->
        raise_err (sprintf "Unterminated string literal (%s)" msg)
    }
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

  | '=' { EQ }
  | "<-" { FEED }

  | '.' { DOT }
  | ',' { COMMA }

  | "$$" { DOLLAR_DOLLAR }
  | "$!" { DOLLAR_NOT }
  | "===" { EQ_EQ_EQ }
  | "!==" { NOT_EQ_EQ }
  | "==" { EQ_EQ }
  | "!=" { NOT_EQ }
  | "&&" { AND }
  | "||" { OR }
  | "??" { NIL_OR }
  | "<" { LT }
  | "<=" { LT_OR_EQ }
  | ">" { GT }
  | ">=" { GT_OR_EQ }

  | "*=" { MUL_EQ }
  | "/=" { DIV_EQ }
  | "+=" { ADD_EQ }
  | "-=" { SUB_EQ }
  (* Other *)
  | _ {  
      raise_err ("Unexpected character in input stream: " ^ Lexing.lexeme lexbuf)
    }
  | eof { EOF }

and read_comment buf = shortest
  | [^ '\n']* as str { COMMENT str }

and read_doc_comment buf = shortest
  | [^ '\n']* as str { DOC_COMMENT str }

and read_double_quoted_string = shortest
  | (([^ '\\'] | '\\' _)* as str) '"' {
      new_lines lexbuf (count_newlines str);
      STRING (process_str str)
    }

and read_single_quoted_string = shortest
  | (([^ '\\'] | '\\' _)* as str) '\'' {
      new_lines lexbuf (count_newlines str);
      STRING (process_str str)
    }
