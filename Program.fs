open Feint.Interpreter

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

    let repl = REPL()
    repl.start ()
