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
%token SCOPE_START                     "->:"
// %token SCOPE_END
%token INLINE_SCOPE_START              "->"
// %token INLINE_SCOPE_END

%token FUNC_START                      "=>:"
%token INLINE_FUNC_START               "=>"

(* Groupings *)
%token LPAREN                          "("
%token RPAREN                          ")"
%token LBRACE                          "["
%token RBRACE                          "]"
%token LBRACKET                        "{"
%token RBRACKET                        "}"

(* Keyword *)
%token NIL                             "nil"
%token TRUE                            "true"
%token FALSE                           "false"
%token BLOCK                           "block"
%token IF                              "if"
%token ELSE                            "else"
%token MATCH                           "match"

(* Types *)
%token <int> INT
%token <float> FLOAT
%token <string> STRING

(* Identifiers *)
%token <string> IDENT
%token <string> SPECIAL_IDENT

%token COMMA                           ","

(* Operators *)
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

(* Logic Operators *)
%token AND                             "&&"
%token OR                              "||"
%token NIL_OR                          "??"

(* Comparison Operators *)
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

(* In Place Operators *)
%token MUL_EQ                          "*="
%token DIV_EQ                          "/="
%token ADD_EQ                          "+="
%token SUB_EQ                          "-= "

(* Assignment Operators *)
%token EQ                              "="
%token FEED                            "<-"

(* Associativity & Precedence (low to high) *)
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

%left DOT

(* Start *)

%type <Ast.program> program
%type <Ast.statement list> list(statement)
%type <Ast.statement> statement
%type <Ast.expr> expr

%start program
%%

program:
  | statements = list(statement); EOF { statements }
  ;

statement:
  | NL { Newline }
  (* Comments *)
  | c = COMMENT { Comment { start = $startpos; content = c } }
  | c = DOC_COMMENT { DocComment { start = $startpos; content = c } }
  (* Expressions *)
  | e = expr; NL { Expr { start = $startpos; expr = e } }
  | e = expr; EOF { Expr { start = $startpos; expr = e } }
  ;

expr:
  (* Types *)
  | NIL { Nil }
  | TRUE { Bool { start = $startpos; value = true } }
  | FALSE { Bool { start = $startpos; value = false } }
  | v = INT { Int { start = $startpos; value = v } }
  | v = FLOAT { Float { start = $startpos; value = v } }
  | v = STRING { String { start = $startpos; value = v } }
  (* Identifiers *)
  | n = IDENT { Ident { start = $startpos; name = n} }
  | n = SPECIAL_IDENT { SpecialIdent { start = $startpos; name = n } }
  (* Unary Operations *)
  | BANG; a = expr { UnaryOp { start = $startpos; op = Not; operand = a } }
  | BANG_BANG; a = expr { UnaryOp { start = $startpos; op = AsBool; operand = a } }
  (* Binary Operations *)
  | a = expr; CARET; b = expr { BinaryOp { start = $startpos; lhs = a; op = Pow; rhs = b } }
  | a = expr; STAR; b = expr { BinaryOp { start = $startpos; lhs = a; op = Mul; rhs = b } }
  | a = expr; SLASH; b = expr { BinaryOp { start = $startpos; lhs = a; op = Div; rhs = b } }
  | a = expr; DOUBLE_SLASH; b = expr { BinaryOp { start = $startpos; lhs = a; op = FloorDiv; rhs = b } }
  | a = expr; PERCENT; b = expr { BinaryOp { start = $startpos; lhs = a; op = Mod; rhs = b } }
  | a = expr; PLUS; b = expr { BinaryOp { start = $startpos; lhs = a; op = Add; rhs = b } }
  | a = expr; DASH; b = expr { BinaryOp { start = $startpos; lhs = a; op = Sub; rhs = b } }
  | a = expr; DOT; b = expr { BinaryOp { start = $startpos; lhs = a; op = Dot; rhs = b } }
  (* Logic Operations *)
  | a = expr; AND; b = expr { BinaryOp { start = $startpos; lhs = a; op = And; rhs = b } }
  | a = expr; OR; b = expr { BinaryOp { start = $startpos; lhs = a; op = Or; rhs = b } }
  | a = expr; NIL_OR; b = expr { BinaryOp { start = $startpos; lhs = a; op = NilOr; rhs = b } }
  (* Comparison Operations *)
  | a = expr; DOLLAR_DOLLAR; b = expr { CompareOp { start = $startpos; lhs = a; op = DollarDollar; rhs = b } }
  | a = expr; DOLLAR_NOT; b = expr { CompareOp { start = $startpos; lhs = a; op = DollarNot; rhs = b } }
  | a = expr; EQ_EQ_EQ; b = expr { CompareOp { start = $startpos; lhs = a; op = EqEqEq; rhs = b } }
  | a = expr; NOT_EQ_EQ; b = expr { CompareOp { start = $startpos; lhs = a; op = NotEqEq; rhs = b } }
  | a = expr; EQ_EQ; b = expr { CompareOp { start = $startpos; lhs = a; op = EqEq; rhs = b } }
  | a = expr; NOT_EQ; b = expr { CompareOp { start = $startpos; lhs = a; op = NotEq; rhs = b } }
  | a = expr; LT; b = expr { CompareOp { start = $startpos; lhs = a; op = LessThan; rhs = b } }
  | a = expr; LT_OR_EQ; b = expr { CompareOp { start = $startpos; lhs = a; op = LessThanEq; rhs = b } }
  | a = expr; GT; b = expr { CompareOp { start = $startpos; lhs = a; op = GreaterThan; rhs = b } }
  | a = expr; GT_OR_EQ; b = expr { CompareOp { start = $startpos; lhs = a; op = GreaterThanEq; rhs = b } }
  (* In Place Operations *)
  | a = expr; MUL_EQ; b = expr { InPlaceOp { start = $startpos; lhs = a; op = MulEq; rhs = b } }
  | a = expr; DIV_EQ; b = expr { InPlaceOp { start = $startpos; lhs = a; op = DivEq; rhs = b } }
  | a = expr; ADD_EQ; b = expr { InPlaceOp { start = $startpos; lhs = a; op = AddEq; rhs = b } }
  | a = expr; SUB_EQ; b = expr { InPlaceOp { start = $startpos; lhs = a; op = SubEq; rhs = b } }
  (* Assignment *)
  | i = IDENT; EQ; v = expr { Assignment { start = $startpos; name = i; value = v } }
  | i = IDENT; FEED; v = expr { Reassignment { start = $startpos; name = i; value = v } }
  (* Blocks *)
  | BLOCK; SCOPE_START; e = expr { Block { start = $startpos; expr = e } }
  | BLOCK; INLINE_SCOPE_START; e = expr { Block { start = $startpos; expr = e } }
  (* Parenthesized Expression *)
  | LPAREN; e = expr; RPAREN { e }
  ;
