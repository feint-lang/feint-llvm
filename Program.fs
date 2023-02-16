open PowerArgs

open Feint.Interpreter
open Feint.ParserUtil

// XXX: This is used as a hacky way to return an exit code from Main()
//      since PowerArgs doesn't seem to support exit codes when using
//      InvokeMain.
exception Exit of int

[<ArgExceptionBehavior(ArgExceptionPolicy.StandardExceptionHandling)>]
type Argv() =
    [<HelpHook>]
    [<ArgDescription("Show this help")>]
    [<ArgShortcut("-h")>]
    [<ArgShortcut("--help")>]
    member val help = false with get, set

    [<ArgDescription("Run code snippet")>]
    [<ArgShortcut("-c")>]
    [<ArgShortcut("--code")>]
    member val code = "" with get, set

    [<ArgDescription("Run file")>]
    [<ArgExistingFile>]
    [<ArgPosition(0)>]
    member val fileName = "" with get, set

    member this.interpret statements =
        match statements with
        | Ok statements ->
            let intepreter = Interpreter false

            try
                intepreter.interpret statements
                0
            with InterpreterErr msg ->
                eprintfn "Error: %s" msg
                1
        | Error msg ->
            // Lex or parse error
            eprintfn "%s" msg
            2

    member this.Main() : unit =
        let exit_code =
            match this.code with
            | null ->
                match this.fileName with
                | null ->
                    let repl = REPL()
                    repl.start ()
                | _ -> this.interpret (parseFile this.fileName)
            | _ -> this.interpret (parseText this.code)

        raise (Exit exit_code)

[<EntryPoint>]
let main argv =
    System.Console.BackgroundColor <- System.ConsoleColor.Black
    System.Console.ForegroundColor <- System.ConsoleColor.White

    try
        let result = PowerArgs.Args.InvokeMain<Argv>(argv)

        match result.Cancelled with
        | true ->
            // This will happen on --help
            0
        | _ ->
            match result.HandledException with
            | null ->
                // XXX: Unreachable?
                254
            | _ ->
                // This will happen when bad args are passed
                255
    with Exit exit_code ->
        exit_code
