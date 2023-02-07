open Printf

type unary_op = NoOp | Negate
type binary_op = Pow | Mul | Div | FloorDiv | Mod | Add | Sub

type expr =
  (* Types *)
  | Nil
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  (* Identifiers *)
  | Ident of string
  | SpecialIdent of string
  (* Operations *)
  | UnaryOp of unary_op * expr
  | BinaryOp of expr * binary_op * expr

type statement =
  | Comment of string
  | DocComment of string
  | Expr of expr
  | Newline

type program = statement list

let display_unary_op op = match op with NoOp -> "" | Negate -> "-"

let display_binary_op op =
  match op with
  | Pow -> "^"
  | Mul -> "*"
  | Div -> "/"
  | FloorDiv -> "//"
  | Mod -> "%"
  | Add -> "+"
  | Sub -> "-"

let rec display_expr expr =
  match expr with
  | Nil -> "nil"
  | Bool b -> sprintf "%b" b
  | Int i -> sprintf "%i" i
  | Float f -> sprintf "%f" f
  | String s -> s
  | Ident n -> n
  | SpecialIdent n -> n
  | UnaryOp (op, expr) ->
      sprintf "%s%s" (display_unary_op op) (display_expr expr)
  | BinaryOp (lhs, op, rhs) ->
      let lhs = display_expr lhs in
      let op = display_binary_op op in
      let rhs = display_expr rhs in
      sprintf "%s %s %s" lhs op rhs

let display_ast statements =
  Printf.printf "Program Statements:\n";
  List.iter
    (fun statement ->
      let s =
        match statement with
        | Comment c -> sprintf "# %s" c
        | DocComment c -> sprintf "// %s" c
        | Expr e -> display_expr e
        | Newline -> ""
      in
      printf "%s\n" s)
    statements
