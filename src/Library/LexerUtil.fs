module Feint.LexerUtil

open FSharp.Text.Lexing

open Feint.Parser

exception LexerErr of string

let raise_err msg = raise (LexerErr msg)
let curr_pos (lexbuf: LexBuffer<_>) = lexbuf.StartPos

let lexeme (lexbuf: LexBuffer<_>) = LexBuffer<_>.LexemeString lexbuf

// Increment line number
let new_line (lexbuf: LexBuffer<_>) =
    lexbuf.StartPos <- lexbuf.StartPos.NextLine

// Increment line number N times
let rec new_lines lexbuf count =
    match count with
    | 0 -> ()
    | _ ->
        new_line lexbuf
        new_lines lexbuf (count - 1)

// Count newlines in string
let count_newlines str = (String.filter ((=) '\n') str).Length

let new_lines_from_lexeme lexbuf =
    new_lines lexbuf (count_newlines (lexeme lexbuf))

// Get current line number as string
let line_no (lexbuf: LexBuffer<_>) = $"{lexbuf.StartPos.Line}"

// Get current column number as string
let col_no (lexbuf: LexBuffer<_>) = $"{lexbuf.StartPos.Column}"

// Format current line and column numbers
let format_pos lexbuf =
    $"line {line_no lexbuf} at column {col_no lexbuf}"

// Keywords
let keywords =
    Map.ofList
        [ ("nil", NIL)
          ("true", TRUE)
          ("false", FALSE)
          ("block", BLOCK)
          ("if", IF)
          ("else", ELSE)
          ("match", MATCH)
          ("$print", PRINT) ]

let get_keyword word =
    match keywords.TryGetValue word with
    | true, token -> token
    | _ -> IDENT word

// Operators
let operators =
    Map.ofList
        [
          // Unary
          ("!", BANG)
          ("!", BANG_BANG)
          // Binary
          ("^", CARET)
          ("*", STAR)
          ("/", SLASH)
          ("//", DOUBLE_SLASH)
          ("%", PERCENT)
          ("+", PLUS)
          ("-", DASH)
          // Short Circuit
          ("&&", AND)
          ("((", OR)
          ("??", NIL_OR)
          // Compare
          ("$$", DOLLAR_DOLLAR)
          ("$!", DOLLAR_NOT)
          ("===", EQ_EQ_EQ)
          ("!==", NOT_EQ_EQ)
          ("==", EQ_EQ)
          ("!=", NOT_EQ)
          ("<", LT)
          ("<=", LT_OR_EQ)
          (">", GT)
          (">=", GT_OR_EQ)
          // In Place
          ("*=", MUL_EQ)
          ("/=", DIV_EQ)
          ("+=", ADD_EQ)
          ("-=", SUB_EQ)
          // Assignment
          ("=", EQ)
          ("<-", FEED)
          // Other
          (".", DOT)
          (",", COMMA) ]

let process_str lexbuf =
    let lex = lexeme lexbuf
    new_lines lexbuf (count_newlines lex)

    // NOTE: Remove trailing quote from lexeme
    let str = lex.Substring(0, lex.Length - 1)
    let peek_str = (str.Substring 1) + "\\"

    let buf = new System.Text.StringBuilder()
    let mutable skip_next_c = false

    for (c, d) in (Seq.zip str peek_str) do
        if c = '\\' then
            // Handle escaped char d.
            skip_next_c <- true

            match d with
            | '\n' -> ()
            | '\\' -> buf.Append('\\') |> ignore // backslash
            | '0' -> buf.Append('\x00') |> ignore // null
            | 'a' -> buf.Append('\a') |> ignore // bell
            | 'b' -> buf.Append('\b') |> ignore // backspace
            | 'f' -> buf.Append('\f') |> ignore // form feed
            | 'n' -> buf.Append('\n') |> ignore // newline
            | 'r' -> buf.Append('\r') |> ignore // carriage return
            | 't' -> buf.Append('\t') |> ignore // tab
            | 'v' -> buf.Append('\v') |> ignore // vertical tab
            // Unescape escaped quotes
            | '"' -> buf.Append('"') |> ignore
            | '\'' -> buf.Append('\'') |> ignore
            // Other escaped chars resolve to the original escape sequence
            | _ ->
                buf.Append('\\') |> ignore
                buf.Append(d) |> ignore

        else if skip_next_c then
            // Skip c because it was escaped and handled above on the
            // previous iteration.
            skip_next_c <- false

        else
            // Handle unescaped char c.
            buf.Append(c) |> ignore

    buf.ToString()
