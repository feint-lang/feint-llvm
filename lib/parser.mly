(** The parser takes tokens produced by the lexer and produces an AST. *)

%{
[@@@coverage exclude_file]

open Ast
%}

(* Whitespace *)
%token NL
%token <int> INDENT
%token EOF

(* Comments *)
%token <string> COMMENT
%token <string> DOC_COMMENT

(* Scopes *)
%token SCOPE_START
%token FUNC_START

(* Groupings *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET

(* Keyword *)
%token NIL
%token TRUE
%token FALSE
%token IF
%token ELSE
%token MATCH

(* Types *)
%token <int> INT
%token <float> FLOAT
%token <string> STRING

(* Identifiers *)
%token <string> IDENT
%token <string> SPECIAL_IDENT

(* Operators *)
%token BANG
%token BANG_BANG
%token CARET
%token STAR
%token SLASH
%token DOUBLE_SLASH
%token PERCENT
%token PLUS
%token DASH
%token DOT

(* Associativity & Precedence (low to high) *)
%left DASH
%left PLUS
%left SLASH
%left DOUBLE_SLASH
%left PERCENT
%left STAR
%right DOT
%right CARET
%nonassoc BANG
%nonassoc BANG_BANG

%type <Ast.program> program

%start program

%%

program:
  | statements = list(statement); EOF { statements }
  ;

statement:
  | NL { Newline }
  | e = expr; NL { Expr e }
  | e = expr; EOF { Expr e }
  ;

expr:
  (* Comments *)
  | c = COMMENT { Comment c }
  | c = DOC_COMMENT { DocComment c }
  (* Types *)
  | NIL { Nil }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { String s }
  (* Identifiers *)
  | n = IDENT { Ident n }
  | n = SPECIAL_IDENT { SpecialIdent n }
  (* Operations *)
  | lhs = expr; CARET; rhs = expr { BinaryOp (lhs, Pow, rhs) }
  | lhs = expr; STAR; rhs = expr { BinaryOp (lhs, Mul, rhs) }
  | lhs = expr; SLASH; rhs = expr { BinaryOp (lhs, Div, rhs) }
  | lhs = expr; DOUBLE_SLASH; rhs = expr { BinaryOp (lhs, FloorDiv, rhs) }
  | lhs = expr; PERCENT; rhs = expr { BinaryOp (lhs, Mod, rhs) }
  | lhs = expr; PLUS; rhs = expr { BinaryOp (lhs, Add, rhs) }
  | lhs = expr; DASH; rhs = expr { BinaryOp (lhs, Sub, rhs) }
  | LPAREN; e = expr; RPAREN { e }
  ;
