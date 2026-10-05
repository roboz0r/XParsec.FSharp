module XParsec.FSharp.Tests.InitializationTests

open System.Diagnostics
open System.Reflection
open Expecto
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let parseCold mode =
    let source =
        match mode with
        | "constructor" ->
            "module M\n[<Struct>]\ntype Cursor<'T> =\n    val private xs: 'T[]\n    internal new(xs: 'T[]) = { xs = xs }\n"
        | "measure" -> "module M\ntype Values<[<Measure>] 'M> = Values of int<'M>\n"
        | "signature" -> "module M\n[<Measure>]\ntype Kg\nval value: int<Kg>\n"
        | _ -> invalidArg (nameof mode) "Unknown cold parser fixture."

    let lexed = Lexing.lexString source
    let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

    let result =
        if mode = "signature" then
            FSharpAst.parseSignature reader
        else
            FSharpAst.parse reader

    match result, reader.State.Diagnostics with
    | Ok _, [] -> 0
    | _ ->
        eprintfn "%A %A" result reader.State.Diagnostics
        1

let private runFresh mode =
    let start =
        ProcessStartInfo(
            "dotnet",
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        )

    start.ArgumentList.Add(Assembly.GetExecutingAssembly().Location)
    start.ArgumentList.Add "--cold-parse"
    start.ArgumentList.Add mode
    use proc = Process.Start start
    let stdout = proc.StandardOutput.ReadToEndAsync()
    let stderr = proc.StandardError.ReadToEndAsync()

    if not (proc.WaitForExit 30_000) then
        proc.Kill true
        failtest "Cold parser process timed out."

    Expect.equal proc.ExitCode 0 (stderr.Result + stdout.Result)

[<Tests>]
let tests =
    testList
        "Cold parser initialization"
        [
            testCase "measure arguments initialize at implementation entry" (fun _ -> runFresh "measure")
            testCase "measure arguments initialize at signature entry" (fun _ -> runFresh "signature")
            testCase "secondary constructors initialize at implementation entry" (fun _ -> runFresh "constructor")
        ]
