open Ast
open Printf

type stack_val = NilVal | BoolVal of bool | IntVal of int | StrVal of string

let stack = Stack.create ()
let push_nil = Stack.push NilVal stack
let push_bool bool = Stack.push (BoolVal bool) stack
let push_int int = Stack.push (IntVal int) stack
let push_str str = Stack.push (StrVal str) stack
let peek = Stack.top stack
let pop = Stack.pop stack

let display_val = function
  | NilVal -> "nil"
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | StrVal s -> s

let display_stack () =
  eprintf "STACK:\n";
  Stack.iter (fun v -> eprintf "%s\n" (display_val v)) stack;
  eprintf "\n"

let interpret_add lhs rhs =
  match (lhs, rhs) with Int a, Int b -> push_int (a.value + b.value) | _ -> push_int 0

let interpret_binary_op lhs op rhs =
  match op with Add -> interpret_add lhs rhs | _ -> Stack.push (IntVal 0) stack

let rec interpret_expr = function
  | Nil _ -> push_nil
  | Bool b -> push_bool b.value
  | Int i -> push_int i.value
  | String s -> push_str s.value
  | BinaryOp op -> interpret_binary_op op.lhs op.op op.rhs
  | Print p ->
      interpret_expr p.expr;
      display_stack ();
      eprintf "%s\n" (display_val pop)
  | _ -> ()

let interpret_statement = function
  | Comment _ -> ()
  | DocComment _ -> ()
  | Expr e -> interpret_expr e.expr
  | Newline -> ()

let interpret statements = List.iter (fun s -> interpret_statement s) statements
