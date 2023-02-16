// Signature file for parser generated by fsyacc
module Feint.Parser
type token = 
  | FEED
  | EQ
  | SUB_EQ
  | ADD_EQ
  | DIV_EQ
  | MUL_EQ
  | GT_OR_EQ
  | GT
  | LT_OR_EQ
  | LT
  | NOT_EQ
  | EQ_EQ
  | NOT_EQ_EQ
  | EQ_EQ_EQ
  | DOLLAR_NOT
  | DOLLAR_DOLLAR
  | NIL_OR
  | OR
  | AND
  | DOT
  | DASH
  | PLUS
  | PERCENT
  | DOUBLE_SLASH
  | SLASH
  | STAR
  | CARET
  | BANG_BANG
  | BANG
  | COMMA
  | SPECIAL_IDENT of (string)
  | IDENT of (string)
  | STR of (string)
  | FLOAT of (float)
  | INT of (bigint)
  | PRINT
  | MATCH
  | ELSE
  | IF
  | BLOCK
  | FALSE
  | TRUE
  | NIL
  | RBRACKET
  | LBRACKET
  | RBRACE
  | LBRACE
  | RPAREN
  | LPAREN
  | INLINE_FUNC_START
  | FUNC_START
  | INLINE_SCOPE_START
  | SCOPE_START
  | DOC_COMMENT of (string)
  | COMMENT of (string)
  | EOF
  | DEDENT
  | INDENT of (int)
  | NL
type tokenId = 
    | TOKEN_FEED
    | TOKEN_EQ
    | TOKEN_SUB_EQ
    | TOKEN_ADD_EQ
    | TOKEN_DIV_EQ
    | TOKEN_MUL_EQ
    | TOKEN_GT_OR_EQ
    | TOKEN_GT
    | TOKEN_LT_OR_EQ
    | TOKEN_LT
    | TOKEN_NOT_EQ
    | TOKEN_EQ_EQ
    | TOKEN_NOT_EQ_EQ
    | TOKEN_EQ_EQ_EQ
    | TOKEN_DOLLAR_NOT
    | TOKEN_DOLLAR_DOLLAR
    | TOKEN_NIL_OR
    | TOKEN_OR
    | TOKEN_AND
    | TOKEN_DOT
    | TOKEN_DASH
    | TOKEN_PLUS
    | TOKEN_PERCENT
    | TOKEN_DOUBLE_SLASH
    | TOKEN_SLASH
    | TOKEN_STAR
    | TOKEN_CARET
    | TOKEN_BANG_BANG
    | TOKEN_BANG
    | TOKEN_COMMA
    | TOKEN_SPECIAL_IDENT
    | TOKEN_IDENT
    | TOKEN_STR
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_PRINT
    | TOKEN_MATCH
    | TOKEN_ELSE
    | TOKEN_IF
    | TOKEN_BLOCK
    | TOKEN_FALSE
    | TOKEN_TRUE
    | TOKEN_NIL
    | TOKEN_RBRACKET
    | TOKEN_LBRACKET
    | TOKEN_RBRACE
    | TOKEN_LBRACE
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_INLINE_FUNC_START
    | TOKEN_FUNC_START
    | TOKEN_INLINE_SCOPE_START
    | TOKEN_SCOPE_START
    | TOKEN_DOC_COMMENT
    | TOKEN_COMMENT
    | TOKEN_EOF
    | TOKEN_DEDENT
    | TOKEN_INDENT
    | TOKEN_NL
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startModule
    | NONTERM_Module
    | NONTERM_StatementList
    | NONTERM_Statement
    | NONTERM_Comment
    | NONTERM_Expr
    | NONTERM_Block
    | NONTERM_Operation
    | NONTERM_Assignment
    | NONTERM_Atom
    | NONTERM_UnaryOp
    | NONTERM_BinaryOp
    | NONTERM_ShortCircuitOp
    | NONTERM_CompareOp
    | NONTERM_InPlaceOp
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Module : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Ast.Statement list) 
