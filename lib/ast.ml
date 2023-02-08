open Lexing
open Printf

type unary_op = NoOp | Negate | Not | AsBool
type binary_op = Pow | Mul | Div | FloorDiv | Mod | Add | Sub | And | Or | NilOr | Dot

type compare_op =
  | DollarDollar
  | DollarNot
  | EqEqEq
  | NotEqEq
  | EqEq
  | NotEq
  | LessThan
  | LessThanEq
  | GreaterThan
  | GreaterThanEq

type in_place_op = MulEq | DivEq | AddEq | SubEq

type expr =
  (* Types *)
  | Nil of { start : position }
  | Bool of { start : position; value : bool }
  | Int of { start : position; value : int }
  | Float of { start : position; value : float }
  | String of { start : position; value : string }
  (* Identifiers *)
  | Ident of { start : position; name : string }
  | SpecialIdent of { start : position; name : string }
  (* Operations *)
  | UnaryOp of { start : position; op : unary_op; operand : expr }
  | BinaryOp of { start : position; lhs : expr; op : binary_op; rhs : expr }
  | CompareOp of { start : position; lhs : expr; op : compare_op; rhs : expr }
  | InPlaceOp of { start : position; lhs : expr; op : in_place_op; rhs : expr }
  (* Assignment *)
  | Assignment of { start : position; name : string; value : expr }
  | Reassignment of { start : position; name : string; value : expr }
  (* Block *)
  | Block of { start : position; expr : expr }

type statement =
  | Comment of { start : position; content : string }
  | DocComment of { start : position; content : string }
  | Expr of { start : position; expr : expr }
  | Newline

type program = statement list

let display_pos pos = sprintf "%i:%i" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let display_unary_op = function
  | NoOp -> ""
  | Negate -> "-"
  | Not -> "!"
  | AsBool -> "!!"

let display_binary_op = function
  | Pow -> "^"
  | Mul -> "*"
  | Div -> "/"
  | FloorDiv -> "//"
  | Mod -> "%"
  | Add -> "+"
  | Sub -> "-"
  | And -> "&&"
  | Or -> "||"
  | NilOr -> "??"
  | Dot -> "."

let display_compare_op = function
  | DollarDollar -> "$$"
  | DollarNot -> "$!"
  | EqEqEq -> "==="
  | NotEqEq -> "!=="
  | EqEq -> "=="
  | NotEq -> "!="
  | LessThan -> "<"
  | LessThanEq -> "<="
  | GreaterThan -> ">"
  | GreaterThanEq -> ">="

let display_in_place_op = function
  | MulEq -> "*="
  | DivEq -> "/="
  | AddEq -> "+="
  | SubEq -> "-="

let rec display_expr expr =
  match expr with
  | Nil _ -> "nil"
  | Bool b -> sprintf "%b" b.value
  | Int i -> sprintf "%i" i.value
  | Float f -> sprintf "%f" f.value
  | String s -> sprintf "\"%s\"" (String.escaped s.value)
  | Ident i -> i.name
  | SpecialIdent i -> i.name
  | UnaryOp op -> sprintf "%s%s" (display_unary_op op.op) (display_expr op.operand)
  | BinaryOp op ->
      let lhs = display_expr op.lhs in
      let rhs = display_expr op.rhs in
      sprintf "%s %s %s" lhs (display_binary_op op.op) rhs
  | CompareOp op ->
      let lhs = display_expr op.lhs in
      let rhs = display_expr op.rhs in
      sprintf "%s %s %s" lhs (display_compare_op op.op) rhs
  | InPlaceOp op ->
      let lhs = display_expr op.lhs in
      let rhs = display_expr op.rhs in
      sprintf "%s %s %s" lhs (display_in_place_op op.op) rhs
  | Assignment a -> sprintf "%s = %s" a.name (display_expr a.value)
  | Reassignment a -> sprintf "%s <- %s" a.name (display_expr a.value)
  | Block b -> sprintf "block -> %s" (display_expr b.expr)

let display_ast statements =
  Printf.printf "Program Statements:\n";
  List.iter
    (fun statement ->
      let s =
        match statement with
        | Comment c -> sprintf "%12s | # %s" (display_pos c.start) c.content
        | DocComment c -> sprintf "%12s | // %s" (display_pos c.start) c.content
        | Expr e -> sprintf "%12s | %s" (display_pos e.start) (display_expr e.expr)
        | Newline -> sprintf "%12s |" ""
      in
      printf "%s\n" s)
    statements
