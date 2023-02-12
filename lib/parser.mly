%{
[@@@coverage exclude_file]

open Ast
%}

// Whitespace ----------------------------------------------------------

%token NL
%token <int> INDENT
%token EOF

// Comments ------------------------------------------------------------

%token <string> COMMENT
%token <string> DOC_COMMENT

// Scopes --------------------------------------------------------------

%token SCOPE_START                     "->:"
// %token SCOPE_END
%token INLINE_SCOPE_START              "->"
// %token INLINE_SCOPE_END

%token FUNC_START                      "=>:"
%token INLINE_FUNC_START               "=>"

// Groupings -----------------------------------------------------------

%token LPAREN                          "("
%token RPAREN                          ")"
%token LBRACE                          "["
%token RBRACE                          "]"
%token LBRACKET                        "{"
%token RBRACKET                        "}"

// Keywords ------------------------------------------------------------

%token NIL                             "nil"
%token TRUE                            "true"
%token FALSE                           "false"
%token BLOCK                           "block"
%token IF                              "if"
%token ELSE                            "else"
%token MATCH                           "match"
%token PRINT                           "$print"

// Types ---------------------------------------------------------------

%token <int> INT
%token <float> FLOAT
%token <string> STRING

// Identifiers ---------------------------------------------------------

%token <string> IDENT
%token <string> SPECIAL_IDENT

%token COMMA                           ","

// Operators -----------------------------------------------------------

%token BANG                            "!"
%token BANG_BANG                       "!!"

%token CARET                           "^"
%token STAR                            "*"
%token SLASH                           "/"
%token DOUBLE_SLASH                    "//"
%token PERCENT                         "%"
%token PLUS                            "+"
%token DASH                            "-"

%token DOT                             "$"

// Logic Operators -----------------------------------------------------

%token AND                             "&&"
%token OR                              "||"
%token NIL_OR                          "??"

// Comparison Operators ------------------------------------------------

%token DOLLAR_DOLLAR                   "$$"
%token DOLLAR_NOT                      "$!"
%token EQ_EQ_EQ                        "==="
%token NOT_EQ_EQ                       "!=="
%token EQ_EQ                           "=="
%token NOT_EQ                          "!="
%token LT                              "<"
%token LT_OR_EQ                        "<="
%token GT                              ">"
%token GT_OR_EQ                        ">="

// In Place Operators --------------------------------------------------

%token MUL_EQ                          "*="
%token DIV_EQ                          "/="
%token ADD_EQ                          "+="
%token SUB_EQ                          "-= "

// Assignment Operators ------------------------------------------------

%token EQ                              "="
%token FEED                            "<-"

// Associativity & Precedence (low to high) ----------------------------

%right EQ
%right FEED

%nonassoc SCOPE_START INLINE_SCOPE_START
%nonassoc FUNC_START INLINE_FUNC_START

%nonassoc ELSE

%left MUL_EQ
%left DIV_EQ
%left ADD_EQ
%left SUB_EQ

%left DOLLAR_DOLLAR
%left DOLLAR_NOT
%left EQ_EQ_EQ
%left NOT_EQ_EQ
%left EQ_EQ
%left NOT_EQ
%left LT
%left LT_OR_EQ
%left GT
%left GT_OR_EQ

%left AND
%left OR
%left NIL_OR

%left DASH
%left PLUS
%left SLASH
%left DOUBLE_SLASH
%left PERCENT
%left STAR

%right CARET

%nonassoc BANG_BANG BANG
%nonassoc LPAREN LBRACE LBRACKET
%nonassoc NIL TRUE FALSE INT FLOAT STRING
%nonassoc PRINT

%left DOT

// Grammar -------------------------------------------------------------

%type <Ast.fmodule> fmodule
%type <Ast.statement list> list(statement)
%type <Ast.statement> statement
%type <Ast.statement> comment
%type <Ast.expr> expr
%type <Ast.expr> block
%type <Ast.expr> operation
%type <Ast.expr> assignment
%type <Ast.expr> ident
%type <Ast.expr> atom

%start fmodule
%%

// Module --------------------------------------------------------------
//
// NOTE: All code is wrapped in a module.

fmodule:
  | statements = list(statement); EOF { statements }
  ;

// Statements ----------------------------------------------------------

statement:
  | NL { Newline }
  | c = comment { c }
  | e = expr; NL { Expr { start = $startpos; expr = e } }
  | e = expr; EOF { Expr { start = $startpos; expr = e } }
  ;

comment:
  | c = COMMENT { Comment { start = $startpos; content = c } }
  | c = DOC_COMMENT { DocComment { start = $startpos; content = c } }

// Expressions ---------------------------------------------------------

expr:
  | b = block { b }
  | o = operation { o }
  | a = assignment { a }
  | a = atom { a }
  | PRINT; e = expr { Print { start = $startpos; expr = e } }
  | LPAREN; e = expr; RPAREN { e }
  ;

block:
  | BLOCK; SCOPE_START; e = expr { Block { start = $startpos; expr = e } }
  | BLOCK; INLINE_SCOPE_START; e = expr { Block { start = $startpos; expr = e } }

operation:
  | op = unary_op; a = expr { UnaryOp { start = $startpos; op = op; operand = a } }
  | a = expr; op = binary_op; b = expr { BinaryOp { start = $startpos; lhs = a; op = op; rhs = b } }
  | a = expr; op = logic_op; b = expr { BinaryOp { start = $startpos; lhs = a; op = op; rhs = b } }
  | a = expr; op = compare_op; b = expr { CompareOp { start = $startpos; lhs = a; op = op; rhs = b } }
  | a = expr; op = in_place_op; b = expr { InPlaceOp { start = $startpos; lhs = a; op = op; rhs = b } }

assignment:
  | i = IDENT; EQ; v = expr { Assignment { start = $startpos; name = i; value = v } }
  | i = IDENT; FEED; v = expr { Reassignment { start = $startpos; name = i; value = v } }

ident:
  | n = IDENT { Ident { start = $startpos; name = n} }
  | n = SPECIAL_IDENT { SpecialIdent { start = $startpos; name = n } }

atom:
  | NIL { Nil { start = $startpos } }
  | TRUE { Bool { start = $startpos; value = true } }
  | FALSE { Bool { start = $startpos; value = false } }
  | v = INT { Int { start = $startpos; value = v } }
  | v = FLOAT { Float { start = $startpos; value = v } }
  | v = STRING { String { start = $startpos; value = v } }
  | i = ident { i }

// Operations ----------------------------------------------------------

%inline unary_op:
  | BANG { Not }
  | BANG_BANG { AsBool }

%inline binary_op:
  | CARET { Pow }
  | STAR { Mul }
  | SLASH { Div }
  | DOUBLE_SLASH { FloorDiv }
  | PERCENT { Mod }
  | PLUS { Add }
  | DASH { Sub }
  | DOT { Dot }

%inline logic_op:
  | AND { And }
  | OR { Or }
  | NIL_OR { NilOr }

%inline compare_op:
  | DOLLAR_DOLLAR { DollarDollar }
  | DOLLAR_NOT { DollarNot }
  | EQ_EQ_EQ { EqEqEq }
  | NOT_EQ_EQ { NotEqEq }
  | EQ_EQ { EqEq }
  | NOT_EQ { NotEq }
  | LT { LessThan }
  | LT_OR_EQ { LessThanEq }
  | GT { GreaterThan }
  | GT_OR_EQ { GreaterThanEq }

%inline in_place_op:
  | MUL_EQ { MulEq }
  | DIV_EQ { DivEq }
  | ADD_EQ { AddEq }
  | SUB_EQ { SubEq }
