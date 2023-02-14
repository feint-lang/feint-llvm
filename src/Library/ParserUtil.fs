module Feint.ParserUtil

open System.IO
open FSharp.Text.Lexing

open Feint

let tryParse lexbuf =
    try
        Ok(Parser.Module Lexer.read lexbuf)
    with
    | LexerUtil.LexerErr msg ->
        let pos = LexerUtil.format_pos lexbuf
        Error $"Syntax error on {pos}: {msg}"
    | _ ->
        let pos = LexerUtil.format_pos lexbuf
        Error $"Parse error on {pos}"

let initLexbuf (lexbuf: LexBuffer<_>) file_name =
    lexbuf.EndPos <-
        { pos_bol = 0
          pos_fname = file_name
          pos_cnum = 1
          pos_lnum = 1
          pos_orig_lnum = 1 }

let parseFile file_name =
    let lexbuf = LexBuffer<_>.FromTextReader(File.OpenText(file_name))
    initLexbuf lexbuf file_name
    tryParse lexbuf

let parseText text =
    let lexbuf = LexBuffer<_>.FromString text
    initLexbuf lexbuf "<text>"
    tryParse lexbuf
