%{
  open Ast
%}

%token NIL
%token TRUE
%token FALSE
%token <int> INT
%token <float> FLOAT
%token <string> STRING

%token STAR
%token SLASH
%token DOUBLE_SLASH
%token PLUS
%token DASH

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET

%token IF
%token ELSE

%token NL
%token <int> INDENT
%token EOF

%left DASH
%left PLUS
%left SLASH
%left DOUBLE_SLASH
%left STAR

%start <Ast.program> program
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
  | NIL { Nil }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | s = STRING { String s }
  | lhs = expr; STAR; rhs = expr { BinaryOp (lhs, Mul, rhs) }
  | lhs = expr; SLASH; rhs = expr { BinaryOp (lhs, Div, rhs) }
  | lhs = expr; DOUBLE_SLASH; rhs = expr { BinaryOp (lhs, FloorDiv, rhs) }
  | lhs = expr; PLUS; rhs = expr { BinaryOp (lhs, Add, rhs) }
  | lhs = expr; DASH; rhs = expr { BinaryOp (lhs, Sub, rhs) }
  | LPAREN; e = expr; RPAREN { e }
  ;
