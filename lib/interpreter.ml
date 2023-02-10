open Ast
open Printf

exception InterpreterErr of string

type stack_val =
  | NilVal
  | BoolVal of bool
  | IntVal of int
  | FloatVal of float
  | StrVal of string

let stack = Stack.create ()
let push_nil = Stack.push NilVal stack
let push_bool b = Stack.push (BoolVal b) stack
let push_int i = Stack.push (IntVal i) stack
let push_float f = Stack.push (FloatVal f) stack
let push_str str = Stack.push (StrVal str) stack

let peek () =
  try Stack.top stack
  with Stack.Empty -> raise (InterpreterErr "Empty stack (cannot peek)")

let pop () =
  try Stack.pop stack
  with Stack.Empty -> raise (InterpreterErr "Empty stack (cannot pop)")

let pop_discard () =
  try
    let _ = Stack.pop stack in
    ()
  with Stack.Empty -> raise (InterpreterErr "Empty stack (cannot pop and discard)")

let display_val = function
  | NilVal -> "nil"
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | StrVal s -> sprintf "\"%s\"" s

let display_stack () =
  eprintf "\nSTACK:\n";
  Stack.iter (fun v -> eprintf "%s\n" (display_val v)) stack;
  eprintf "\n";
  flush stderr

let rec int_pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = int_pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let interpret_pow lhs rhs =
  match (lhs, rhs) with
  | IntVal a, IntVal b -> push_int (int_pow a b)
  | FloatVal a, FloatVal b -> push_float (a ** b)
  | _ ->
      let lhs = display_val lhs in
      let rhs = display_val rhs in
      let msg = sprintf "Cannot multiply %s and %s" lhs rhs in
      raise (InterpreterErr msg)

let interpret_mul lhs rhs =
  match (lhs, rhs) with
  | IntVal a, IntVal b -> push_int (Int.mul a b)
  | FloatVal a, FloatVal b -> push_float (Float.mul a b)
  | _ ->
      let lhs = display_val lhs in
      let rhs = display_val rhs in
      let msg = sprintf "Cannot multiply %s and %s" lhs rhs in
      raise (InterpreterErr msg)

let interpret_div lhs rhs =
  match (lhs, rhs) with
  | IntVal a, IntVal b -> push_int (Int.div a b)
  | FloatVal a, FloatVal b -> push_float (Float.div a b)
  | _ ->
      let lhs = display_val lhs in
      let rhs = display_val rhs in
      let msg = sprintf "Cannot divide %s by %s" lhs rhs in
      raise (InterpreterErr msg)

let interpret_add lhs rhs =
  match (lhs, rhs) with
  | IntVal a, IntVal b -> push_int (Int.add a b)
  | FloatVal a, FloatVal b -> push_float (Float.add a b)
  | _ ->
      let lhs = display_val lhs in
      let rhs = display_val rhs in
      let msg = sprintf "Cannot add %s to %s" lhs rhs in
      raise (InterpreterErr msg)

let interpret_sub lhs rhs =
  match (lhs, rhs) with
  | IntVal a, IntVal b -> push_int (Int.sub a b)
  | FloatVal a, FloatVal b -> push_float (Float.sub a b)
  | _ ->
      let lhs = display_val lhs in
      let rhs = display_val rhs in
      let msg = sprintf "Cannot subtract %s from %s" rhs lhs in
      raise (InterpreterErr msg)

let rec interpret_expr = function
  | Nil _ -> push_nil
  | Bool b -> push_bool b.value
  | Int i -> push_int i.value
  | String s -> push_str s.value
  | BinaryOp op -> interpret_binary_op op.lhs op.op op.rhs
  | Print p ->
      interpret_expr p.expr;
      printf "%s\n" (display_val (pop ()))
  | _ -> ()

and interpret_binary_op lhs op rhs =
  interpret_expr lhs;
  interpret_expr rhs;
  let rhs = pop () in
  let lhs = pop () in
  match op with
  | Pow -> interpret_pow lhs rhs
  | Mul -> interpret_mul lhs rhs
  | Div -> interpret_div lhs rhs
  | Add -> interpret_add lhs rhs
  | Sub -> interpret_sub lhs rhs
  | op ->
      let msg = sprintf "Unhandled binary op: %s" (display_binary_op op) in
      raise (InterpreterErr msg)

let interpret_statement statement show_result =
  match statement with
  | Comment _ -> ()
  | DocComment _ -> ()
  | Newline -> ()
  | Expr e -> (
      flush stderr;
      interpret_expr e.expr;
      match show_result with
      | true ->
          let top = pop () in
          printf "%s\n" (display_val top)
      | false -> pop_discard ())

let interpret_statements statements show_result =
  List.iter (fun statement -> interpret_statement statement show_result) statements

let handle_line line show_result =
  match line with
  | Some line -> (
      match line with
      | ".exit" -> exit 0
      | ".stack" ->
          display_stack ();
          flush stderr
      | _ -> (
          let maybe_statements = Driver.parse_text line in
          match maybe_statements with
          | Ok statements ->
              interpret_statements statements show_result;
              flush stdout
          | _ -> eprintf "Could not parse line: %s\n" line))
  | None -> exit 0

let rec main_loop show_result =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         eprintf "\nUse Ctrl-D (EOF) or .exit to exit\n";
         flush stderr;
         main_loop show_result));

  printf "> ";
  flush stdout;

  let ic = stdin in
  handle_line (In_channel.input_line ic) show_result;
  main_loop show_result
