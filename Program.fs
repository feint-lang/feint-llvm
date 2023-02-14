open System

open Feint.Interpreter
open Feint.ParserUtil

[<EntryPoint>]
let main args =
    // let statements =
    //     [ ExprStatement(Nil)
    //       ExprStatement(Bool true)
    //       ExprStatement(Bool false)
    //       ExprStatement(Int(bigint 1))
    //       ExprStatement(Print([ Bool true ]))
    //       ExprStatement(Float 2)
    //       ExprStatement(Block [ ExprStatement(Nil) ])
    //       ExprStatement(
    //           Block
    //               [ ExprStatement(Bool true)
    //                 ExprStatement(Bool false)
    //                 ExprStatement(Block [ ExprStatement(Nil) ]) ]
    //       ) ]

    // print_statements statements

    if args.Length = 1 then
        match parseFile args.[0] with
        | Ok statements ->
            let i = Interpreter false
            i.interpret statements
            0
        | Error msg ->
            Console.Error.WriteLine msg
            -1
    else
        let repl = REPL()
        repl.start ()
