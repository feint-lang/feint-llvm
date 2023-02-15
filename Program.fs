open PowerArgs

open Feint.Interpreter
open Feint.ParserUtil

[<ArgExceptionBehavior(ArgExceptionPolicy.StandardExceptionHandling)>]
type Argv() =
    // XXX: <HelpHook> causes an exception, so we have to handle help
    //      manually for now
    // [<HelpHook>]
    [<ArgDescription("Show this help")>]
    [<ArgShortcut("-h")>]
    member val Help = false with get, set

    [<ArgDescription("Run code snippet")>]
    [<ArgDefaultValue("")>]
    member val Code = "" with get, set

    [<ArgDescription("Run file")>]
    // [<ArgExistingFile>]
    [<ArgPosition(0)>]
    member val FileName = "" with get, set

let usage () =
    // XXX: This throws an exception
    // let usage = ArgUsage.GenerateUsageFromTemplate<Argv>()

    sprintf "usage: dotnet run [-c <code>] [-f <file_name>]"

let interpret =
    function
    | Ok statements ->
        let intepreter = Interpreter false

        try
            intepreter.interpret statements
            0
        with InterpreterErr msg ->
            eprintfn "%s" msg
            -1
    | Error msg ->
        eprintfn "%s" msg
        -1

let invoke (args: Argv) =
    if args.Help then
        eprintfn "%s" (usage ())
        0
    else if args.Code.Length > 0 then
        interpret (parseText args.Code)
    else if args.FileName.Length > 0 then
        interpret (parseFile args.FileName)
    else
        let repl = REPL()
        repl.start ()

[<EntryPoint>]
let main argv =
    try
        // XXX: This throws an exception
        // PowerArgs.Args.InvokeMain<Argv>(argv) |> ignore

        let args = PowerArgs.Args.Parse<Argv>(argv)
        invoke args
    with :? PowerArgs.ArgException as exc ->
        eprintfn "%s\n%s" exc.Message (usage ())
        -1
