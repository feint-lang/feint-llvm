%{
  open Ast
%}

%token NIL
%token TRUE
%token FALSE
%token <int> INT
%token <string> STRING
%token NL
%token EOF

%start <Ast.program> program
%%

program:
  | statements = list(statement); EOF { statements }
  ;

statement:
  | e = expr; NL { e }
  | e = expr; EOF { e }
  ;

expr:
  | NIL { Nil }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = INT { Int i }
  | s = STRING { String s }
  ;
