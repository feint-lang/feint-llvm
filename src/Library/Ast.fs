module Feint.Ast

// Operations ----------------------------------------------------------

type OpUnary =
    | NoOp
    | Negate
    | Not
    | AsBool

type OpBinary =
    | Pow
    | Mul
    | Div
    | FloorDiv
    | Mod
    | Add
    | Sub
    | Dot

type OpShortCircuit =
    | And
    | Or
    | NilOr

type OpCompare =
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

type OpInPlace =
    | MulEq
    | DivEq
    | AddEq
    | SubEq

// AST -----------------------------------------------------------------

type Statement =
    | Comment of string
    | DocComment of string
    | Return of Expr option
    | ExprStatement of Expr
    | Newline

and Expr =
    | Nil
    | Bool of bool
    | Int of bigint
    | Float of float
    | Str of string
    // Identifiers
    | Ident of string
    | SpecialIdent of string
    // Operations
    | UnaryOp of UnaryOp
    | BinaryOp of BinaryOp
    | ShortCircuitOp of ShortCircuitOp
    | CompareOp of CompareOp
    | InPlaceOp of InPlaceOp
    // Assignment
    | Assignment of Assignment
    | Reassignment of Reassignment
    // Block
    | Block of Statement list
    // Print
    | Print of Expr list

and UnaryOp = { op: OpUnary; rhs: Expr }
and BinaryOp = { lhs: Expr; op: OpBinary; rhs: Expr }

and ShortCircuitOp =
    { lhs: Expr
      op: OpShortCircuit
      rhs: Expr }

and CompareOp = { lhs: Expr; op: OpCompare; rhs: Expr }
and InPlaceOp = { lhs: Expr; op: OpInPlace; rhs: Expr }
and Assignment = { name: string; value: Expr }
and Reassignment = { name: string; value: Expr }

// Display -------------------------------------------------------------

let format_unary_op =
    function
    | NoOp -> "+"
    | Negate -> "-"
    | Not -> "!"
    | AsBool -> "!!"

let format_binary_op =
    function
    | Pow -> "^"
    | Mul -> "*"
    | Div -> "/"
    | FloorDiv -> "//"
    | Mod -> "%"
    | Add -> "+"
    | Sub -> "-"
    | Dot -> "."

let format_short_circuit_op =
    function
    | And -> "&&"
    | Or -> "||"
    | NilOr -> "??"

let format_compare_op =
    function
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

let format_in_place_op =
    function
    | MulEq -> "*="
    | DivEq -> "/="
    | AddEq -> "+="
    | SubEq -> "-="

let make_indent level = String.replicate (level * 4) " "

let rec format_statements statements level =
    let formatter = fun statement -> format_statement statement level
    let statements = List.map formatter statements
    String.concat "" statements

and format_statement statement level =
    let indent = make_indent level

    let result =
        match statement with
        | Comment content -> content
        | DocComment content -> content
        | Return maybe_expr ->
            match maybe_expr with
            | Some expr ->
                let expr = (format_expr expr level)
                $"return {expr}"
            | None -> "return"
        | ExprStatement expr -> (format_expr expr level)
        | Newline -> ""

    $"{indent}{result.TrimEnd()}\n"

and format_expr expr level =
    let indent = make_indent level

    match expr with
    // Types
    | Nil -> "nil"
    | Bool v -> v.ToString().ToLower()
    | Int v -> v.ToString()
    | Float v -> v.ToString()
    | Str v -> v
    // Operations
    | UnaryOp op -> $"{format_unary_op op.op}{format_expr op.rhs level}"
    | BinaryOp op -> $"{format_bin_op_expr op.lhs (format_binary_op op.op) op.rhs}"
    | ShortCircuitOp op -> $"{format_bin_op_expr op.lhs (format_short_circuit_op op.op) op.rhs}"
    | CompareOp op -> $"{format_bin_op_expr op.lhs (format_compare_op op.op) op.rhs}"
    | InPlaceOp op -> $"{format_bin_op_expr op.lhs (format_in_place_op op.op) op.rhs}"
    // Assignments
    | Ident name -> name
    | SpecialIdent name -> name
    // Blocks
    | Block statements ->
        let statements = format_statements statements (level + 1)
        $"\n{indent}block ->\n{statements}"
    // Print
    | Print args ->
        let formatter = fun expr -> format_expr expr 0
        let args = String.concat ", " (List.map formatter args)
        $"$print {args}"
    | _ -> "unknown expr"

and format_bin_op_expr lhs op rhs level =
    $"{format_expr lhs level} {op} {format_expr rhs level}"

let print_statements statements =
    let statements = (format_statements statements 0)
    printfn $"{statements.TrimEnd()}"
