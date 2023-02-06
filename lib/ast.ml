type unary_op = NoOp | Negate
type binary_op = Mul | Div | FloorDiv | Add | Sub

type expr =
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | UnaryOp of unary_op * expr
  | BinaryOp of expr * binary_op * expr
  | DocComment of string

type statement = Jump | Expr of expr | Newline
type program = statement list
