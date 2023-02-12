open Ast
open Printf

exception InterpreterErr of string

let raise_err msg = raise (InterpreterErr msg)

let raise_err_with_pos pos msg =
  let msg = sprintf "%s: %s" (display_pos pos) msg in
  raise_err msg

type stack_val =
  | NilVal
  | BoolVal of bool
  | IntVal of int
  | FloatVal of float
  | StrVal of string

let display_val = function
  | NilVal -> "nil"
  | BoolVal b -> string_of_bool b
  | IntVal i -> string_of_int i
  | FloatVal f -> string_of_float f
  | StrVal s -> sprintf "%s" s

let debug_val = function StrVal s -> sprintf "\"%s\"" s | v -> display_val v

let rec int_pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = int_pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

(* Interpreter ------------------------------------------------------ *)

class interpreter show_statement_result =
  object (self)
    val stack = Stack.create ()
    val names = Hashtbl.create 32
    val show_statement_result = show_statement_result
    method push v = Stack.push v stack
    method push_nil = Stack.push NilVal stack
    method push_bool b = Stack.push (BoolVal b) stack
    method push_true = Stack.push (BoolVal true) stack
    method push_false = Stack.push (BoolVal false) stack
    method push_int i = Stack.push (IntVal i) stack
    method push_float f = Stack.push (FloatVal f) stack
    method push_str str = Stack.push (StrVal str) stack

    method peek =
      try Stack.top stack with Stack.Empty -> raise_err "Empty stack (cannot peek)"

    method pop =
      try Stack.pop stack with Stack.Empty -> raise_err "Empty stack (cannot pop)"

    method pop_discard =
      try
        let _ = Stack.pop stack in
        ()
      with Stack.Empty -> raise_err "Empty stack (cannot pop)"

    method display_stack =
      match Stack.is_empty stack with
      | true -> prerr_endline "[EMPTY]"
      | false -> Stack.iter (fun v -> prerr_endline (debug_val v)) stack

    method interpret statements =
      List.iter (fun statement -> self#interpret_statement statement) statements

    method interpret_statement statement =
      match statement with
      | Comment _ -> ()
      | DocComment _ -> ()
      | Newline -> ()
      | Expr e ->
          self#interpret_expr e.expr;
          let top = self#pop in
          if show_statement_result && top <> NilVal then
            prerr_endline (sprintf "-> %s" (debug_val top))

    method interpret_expr expr =
      match expr with
      | Nil _ -> self#push_nil
      | Bool b -> self#push_bool b.value
      | Int i -> self#push_int i.value
      | Float f -> self#push_float f.value
      | String s -> self#push_str s.value
      | Ident i -> (
          try
            let v = Hashtbl.find names i.name in
            self#push v
          with Not_found -> raise_err (sprintf "Name not found: %s" i.name))
      | Assignment a ->
          self#interpret_expr a.value;
          Hashtbl.add names a.name self#peek
      | Reassignment a -> (
          try
            let _old_v = Hashtbl.find names a.name in
            self#interpret_expr a.value;
            let new_v = self#peek in
            Hashtbl.add names a.name new_v
          with Not_found ->
            raise_err_with_pos a.start
              (sprintf "Cannot reassign %s because it's not already assigned" a.name))
      | BinaryOp op -> self#interpret_binary_op op.lhs op.op op.rhs
      | ShortCircuitingBinaryOp op ->
          self#interpret_short_circuiting_binary_op op.lhs op.op op.rhs
      | CompareOp op -> self#interpret_compare_op op.lhs op.op op.rhs
      | Block b -> self#interpret_expr b.expr
      | Print p ->
          self#interpret_expr p.expr;
          print_endline (display_val self#pop);
          self#push_nil
      | expr -> raise_err (sprintf "Unhandled expression: %s" (display_expr expr))

    (* Binary Operations -------------------------------------------- *)

    method interpret_binary_op lhs op rhs =
      self#interpret_expr lhs;
      self#interpret_expr rhs;
      let rhs = self#pop in
      let lhs = self#pop in
      match op with
      | Pow -> self#interpret_pow lhs rhs
      | Mul -> self#interpret_mul lhs rhs
      | Div -> self#interpret_div lhs rhs
      | Add -> self#interpret_add lhs rhs
      | Sub -> self#interpret_sub lhs rhs
      | op -> raise_err (sprintf "Unhandled binary op: %s" (display_binary_op op))

    method interpret_pow lhs rhs =
      match (lhs, rhs) with
      | IntVal a, IntVal b -> self#push_int (int_pow a b)
      | FloatVal a, FloatVal b -> self#push_float (a ** b)
      | _ ->
          raise_err
            (sprintf "Cannot multiply %s and %s" (debug_val lhs) (debug_val rhs))

    method interpret_mul lhs rhs =
      match (lhs, rhs) with
      | IntVal a, IntVal b -> self#push_int (Int.mul a b)
      | FloatVal a, FloatVal b -> self#push_float (Float.mul a b)
      | _ ->
          raise_err
            (sprintf "Cannot multiply %s and %s" (debug_val lhs) (debug_val rhs))

    method interpret_div lhs rhs =
      match (lhs, rhs) with
      | IntVal a, IntVal b -> self#push_int (Int.div a b)
      | FloatVal a, FloatVal b -> self#push_float (Float.div a b)
      | _ ->
          raise_err (sprintf "Cannot divide %s by %s" (debug_val lhs) (debug_val rhs))

    method interpret_add lhs rhs =
      match (lhs, rhs) with
      | IntVal a, IntVal b -> self#push_int (Int.add a b)
      | FloatVal a, FloatVal b -> self#push_float (Float.add a b)
      | _ -> raise_err (sprintf "Cannot add %s to %s" (debug_val lhs) (debug_val rhs))

    method interpret_sub lhs rhs =
      match (lhs, rhs) with
      | IntVal a, IntVal b -> self#push_int (Int.sub a b)
      | FloatVal a, FloatVal b -> self#push_float (Float.sub a b)
      | _ ->
          raise_err
            (sprintf "Cannot subtract %s from %s" (debug_val lhs) (debug_val rhs))

    (* Short Circuiting Binary Operations --------------------------- *)

    method interpret_short_circuiting_binary_op lhs op rhs =
      self#interpret_expr lhs;
      let lhs = self#pop in
      match op with
      | And -> self#interpret_and lhs rhs
      | Or -> self#interpret_or lhs rhs
      | NilOr -> self#interpret_nil_or lhs rhs

    method interpret_and lhs rhs =
      match lhs with
      | BoolVal true -> (
          self#interpret_expr rhs;
          let rhs = self#pop in
          match rhs with
          | BoolVal true -> self#push_true
          | BoolVal false -> self#push_false
          | _ ->
              raise_err
                (sprintf "Cannot apply %s && %s" (debug_val lhs) (debug_val rhs)))
      | BoolVal false -> self#push_false
      | _ -> raise_err (sprintf "Cannot apply && to %s" (debug_val lhs))

    method interpret_or lhs rhs =
      match lhs with
      | BoolVal true -> self#push_true
      | BoolVal false -> (
          self#interpret_expr rhs;
          let rhs = self#pop in
          match rhs with
          | BoolVal true -> self#push_true
          | BoolVal false -> self#push_false
          | _ ->
              raise_err
                (sprintf "Cannot apply %s || %s" (debug_val lhs) (debug_val rhs)))
      | _ -> raise_err (sprintf "Cannot apply || to %s" (debug_val lhs))

    method interpret_nil_or lhs rhs =
      match lhs with NilVal -> self#interpret_expr rhs | _ -> self#push lhs

    (* Comparison Operations ---------------------------------------- *)

    method interpret_compare_op lhs op rhs =
      self#interpret_expr lhs;
      self#interpret_expr rhs;
      let rhs = self#pop in
      let lhs = self#pop in
      let result =
        match op with
        | EqEq -> self#interpret_eq lhs rhs
        | op ->
            raise_err (sprintf "Unhandled comparison op: %s" (display_compare_op op))
      in
      self#push_bool result

    method interpret_eq lhs rhs =
      match (lhs, rhs) with
      | NilVal, NilVal -> true
      | BoolVal a, BoolVal b -> a == b
      | IntVal a, IntVal b -> a == b
      | FloatVal a, FloatVal b -> a == b
      | StrVal a, StrVal b -> a == b
      | _ -> false
  end

(* REPL ------------------------------------------------------------- *)

class repl =
  object (self)
    val interpreter = new interpreter true
    val mutable history_path : string option = None

    method start =
      [
        "Welcome to the Feint REPL";
        "Type a line of code, then hit Enter to evaluate it";
        "Enter .exit or .quit to exit";
      ]
      |> List.iter prerr_endline;

      self#configure_history;
      self#loop

    method loop =
      match self#read_line with
      | Some line ->
          self#handle_line line;
          self#loop
      | None -> ()

    method read_line =
      try LNoise.linenoise "#> "
      with Sys.Break ->
        prerr_endline "Use Ctrl-D to exit";
        self#read_line

    method handle_line line =
      self#save_history_entry line;
      let is_command = String.starts_with ~prefix:"." line || line = "?" in
      match is_command with
      | true -> self#handle_command line
      | false -> self#interpret_line line

    method handle_command =
      function
      | ".help" | "?" ->
          [
            "Feint Help\n";
            ".help | ? -> show this help";
            ".exit -> exit";
            ".stack -> show interpreter stack (top first)";
          ]
          |> List.iter prerr_endline
      | ".exit" | ".quit" -> exit 0
      | ".stack" -> interpreter#display_stack
      | command -> prerr_endline (sprintf "Unknown REPL command: %s" command)

    method interpret_line line =
      match Driver.parse_text line with
      | Ok statements -> (
          try interpreter#interpret statements
          with InterpreterErr msg -> prerr_endline msg)
      | Error _ -> Driver.handle_err "<repl>" line

    method configure_history =
      let rel_path = ".config/feint/repl-history" in
      try
        let home = Sys.getenv "HOME" in
        let path = sprintf "%s/%s" home rel_path in
        LNoise.history_set ~max_length:512 |> ignore;
        LNoise.history_load ~filename:path |> ignore;
        history_path <- Some path
      with Not_found ->
        prerr_endline (sprintf "Could not load history from ${HOME}/%s" rel_path)

    method save_history_entry entry =
      match history_path with
      | Some path ->
          LNoise.history_add entry |> ignore;
          LNoise.history_save ~filename:path |> ignore
      | None -> ()
  end
