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
let push_nil () = Stack.push NilVal stack
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

let display_val = function
  | NilVal -> "nil"
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | StrVal s -> sprintf "\"%s\"" s

let display_stack () =
  prerr_endline "\nSTACK:\n";
  Stack.iter (fun v -> prerr_endline (display_val v)) stack;
  prerr_endline ""

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
  | Nil _ -> push_nil ()
  | Bool b -> push_bool b.value
  | Int i -> push_int i.value
  | String s -> push_str s.value
  | BinaryOp op -> interpret_binary_op op.lhs op.op op.rhs
  | Print p ->
      interpret_expr p.expr;
      print_endline (display_val (pop ()));
      push_nil ()
  | expr ->
      let msg = sprintf "Unhandled expression: %s" (display_expr expr) in
      raise (InterpreterErr msg)

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
  | Expr e ->
      interpret_expr e.expr;
      let top = pop () in
      if show_result && top <> NilVal then
        prerr_endline (sprintf "-> %s" (display_val top))

let interpret_statements statements show_result =
  List.iter (fun statement -> interpret_statement statement show_result) statements

(* REPL ------------------------------------------------------------- *)

let configure_history () =
  let rel_path = ".config/feint/repl-history" in
  try
    let home = Sys.getenv "HOME" in
    let path = sprintf "%s/%s" home rel_path in
    LNoise.history_set ~max_length:512 |> ignore;
    LNoise.history_load ~filename:path |> ignore;
    Some path
  with Not_found ->
    prerr_endline (sprintf "Could not load history from ${HOME}/%s" rel_path);
    None

let save_history_entry entry = function
  | Some path ->
      LNoise.history_add entry |> ignore;
      LNoise.history_save ~filename:path |> ignore
  | None -> ()

let interpret_line line history_path =
  match Driver.parse_text line with
  | Ok statements -> (
      save_history_entry line history_path;
      try interpret_statements statements true
      with InterpreterErr msg -> prerr_endline msg)
  | Error _ -> Driver.handle_err "<repl>" line

let show_help () =
  [
    "Feint Help\n";
    ".help | ? -> show this help";
    ".exit -> exit";
    ".stack -> show interpreter stack (top first)";
  ]
  |> List.iter prerr_endline

let handle_line line history_path =
  match line with
  | ".help" | "?" -> show_help ()
  | ".exit" | ".quit" -> exit 0
  | ".stack" -> display_stack ()
  | _ -> interpret_line line history_path

let rec read_line () =
  try LNoise.linenoise "#> "
  with Sys.Break ->
    prerr_endline "Use Ctrl-D to exit";
    read_line ()

let rec repl_loop history_path =
  match read_line () with
  | Some line ->
      handle_line line history_path;
      repl_loop history_path
  | None -> exit 0

let start_repl () =
  [
    "Welcome to the Feint REPL";
    "Type a line of code, then hit Enter to evaluate it";
    "Enter .exit or .quit to exit";
  ]
  |> List.iter prerr_endline;
  repl_loop (configure_history ())
