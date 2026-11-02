module XParsec.FSharp.Tests.ParserTests

open System
open System.IO

open Expecto

let specialFiles =
    [
        // These files require special handling because they contain `#if` directives
        // that cause the parser to produce different outputs based on which symbols are defined.
        "24_if_directive.fs", [ "A" ]
    ]

/// Parser tests are discovered automatically from `.fs` source files.
/// To add a new test: create the `.fs` source file, run once locally (it will create the golden
/// file and fail), verify the output is correct, then commit both files.
/// To regenerate all golden files after a format change: set UPDATE_SNAPSHOTS=1 and re-run.
[<Tests>]
let tests =
    testList
        "ParserTests"
        [
            for path in testData.Value do
                let fileName = Path.GetFileName path
                let name = $"Parsing {fileName}"
                test name { testParseFile path }

            for (fileName, symbols) in specialFiles do
                let path = Path.Combine(testDataDir.Value, fileName)
                let name = $"""Parsing {fileName} ({String.concat ", " symbols})"""
                test name { testParseFileWithSymbols symbols path }

            ptest "Step Debugging Test" {
                // This test is intended for manual use with a debugger to verify that stepping
                // through the parser works correctly.
                // Change the file path to point to a specific test file you want to debug, change to `ftest`, then set a breakpoint in the parser.
                let path = Path.Combine(testDataDir.Value, "manual", "file.fs")
                testParseFile path
            }

            ptest "Trace Debugging Test" {
                // This test will print detailed trace events from the parser into file.fs.trace and file.fs.stack files
                // which can be useful for debugging complex parsing issues.
                let path = Path.Combine(testDataDir.Value, "manual", "file.fs")
                // 1MB is default stack size for .NET, which should be sufficient for most parsing tasks.
                // Increase if necessary for debugging stack overflows.
                // 10 second timeout to prevent hanging indefinitely if there is an infinite loop or other issue.
                let stackSize = 0x100000
                let result = parseWithStackProbe stackSize (System.TimeSpan.FromSeconds 10.0) path

                match result with
                | Error e ->
                    failtestf "Parsing failed:\n%s" (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                | Ok _ -> ()
            }
        ]

/// Signature-file (`.fsi`) parser tests, discovered automatically from `.fsi` files in `data/`.
/// To add a new test: create the `.fsi` source file, run once locally (it will create the golden
/// file and fail), verify the output is correct, then commit both files.
[<Tests>]
let signatureTests =
    testList
        "SignatureTests"
        [
            for path in sigTestData.Value do
                let fileName = Path.GetFileName path
                let name = $"Parsing {fileName}"
                test name { testParseSignatureFile path }
        ]

/// FSharp.Core corpus pass: every .fsi under `test/.../data/fsharp-core-fsi/` must parse Ok
/// (with zero recovery diagnostics). The corpus is a copy of `dotnet/fsharp` FSharp.Core .fsi files.
[<Tests>]
let fsharpCoreCorpusTests =
    let corpusDir = Path.Combine(testDataDir.Value, "fsharp-core-fsi")
    let files = findSignatureCorpusFiles corpusDir

    testList
        "FSharpCoreCorpus"
        [
            for path in files do
                let relPath = Path.GetRelativePath(corpusDir, path)
                let name = $"Sig parse {relPath}"

                test name {
                    match tryParseSignatureCorpusFile path with
                    | Success(0, _) -> ()
                    | Success(n, diag) -> failtestf "Parsed with %d diagnostic(s):\n%s" n diag
                    | ParseError msg -> failtestf "Parse failed:\n%s" msg
                    | ParseException ex -> failtestf "Exception %s: %s" (ex.GetType().Name) ex.Message
                    | Timeout -> failtest "Timed out (>30s)"
                }
        ]

[<Tests>]
let integrityTests =
    testList
        "GoldenFileIntegrity"
        [
            test "No orphaned golden files in data/" {
                let orphans = findOrphanedGoldenFiles testDataDir.Value

                if orphans.Length > 0 then
                    let fileList = orphans |> String.concat "\n  "

                    failtestf
                        "Found %d orphaned golden file(s) with no corresponding .fs source:\n  %s"
                        orphans.Length
                        fileList
            }
        ]

[<Tests>]
let tracingTests =
    testList
        "TracingTests"
        [
            test "Tracing emits ContextPush events" {
                let lexed = XParsec.FSharp.Lexer.Lexing.lexString "let x = 1"
                let mutable pushCount = 0
                let mutable consumeCount = 0

                let traceCallback =
                    { new XParsec.FSharp.Parser.TraceCallback() with
                        override _.ContextPush(_, _, _, _) = pushCount <- pushCount + 1
                        override _.TokenConsumed(_, _, _) = consumeCount <- consumeCount + 1
                    }

                let reader = ofParseInputWithTracing (lexed.WithDefines Set.empty) traceCallback

                match XParsec.FSharp.Parser.FSharpAst.parse reader with
                | Error e ->
                    failtestf "Parsing failed:\n%s" (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                | Ok _ ->
                    Expect.isGreaterThan pushCount 0 "Expected at least one ContextPush event"
                    Expect.isGreaterThan consumeCount 0 "Expected at least one TokenConsumed event"
            }

            test "Tracing is off by default" {
                let lexed = XParsec.FSharp.Lexer.Lexing.lexString "let x = 1"
                let reader = XParsec.FSharp.Parser.Reader.ofParseInput (lexed.WithDefines Set.empty)

                // Should parse successfully with default (no-op) tracing
                match XParsec.FSharp.Parser.FSharpAst.parse reader with
                | Error e ->
                    failtestf "Parsing failed:\n%s" (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                | Ok _ -> ()
            }
        ]

/// Whether `source` parses without an error or a recovery diagnostic.
let private parsesClean (source: string) : bool =
    let lexed = XParsec.FSharp.Lexer.Lexing.lexString source
    let reader = XParsec.FSharp.Parser.Reader.ofParseInput (lexed.WithDefines Set.empty)

    match XParsec.FSharp.Parser.FSharpAst.parse reader with
    | Ok _ -> Seq.isEmpty reader.State.Diagnostics
    | Error _ -> false

[<Tests>]
let inlineBindingTests =
    testList
        "InlineBindings"
        [
            // fsc parses an inline value, reporting FS0832 after parsing, and refuses explicit type
            // parameters after a pattern head that is other than a name (FS0010 at `<`).
            test "an inline value parses, and an inline pattern head with explicit type parameters is refused" {
                Expect.isTrue (parsesClean "let inline v = 1\n") "an inline value"
                Expect.isTrue (parsesClean "let inline (a, b) = (1, 2)\n") "an inline tuple value"
                Expect.isTrue (parsesClean "let inline v<'T> = 1\n") "an inline generic value"

                let refused =
                    [
                        "let inline (x, y)<'T> = (1, 2)\n"
                        "let inline _<'T> = 1\n"
                        "let inline (x)<'T> = 1\n"
                        "let inline mutable m = 1\n"
                    ]

                Expect.equal
                    [ for source in refused -> source, parsesClean source ]
                    [ for source in refused -> source, false ]
                    "each source is refused"
            }
        ]

/// The role of an operator at the start of a line in a sequential block.
type private LineStartOperator =
    /// The operator continues the preceding expression.
    | Infix
    /// The operator begins a new expression of the block.
    | NewExpression

/// Parses `source` and classifies the operator that starts line `line` (1-based) by the CST node
/// at its position: an infix application or assignment, or a prefix application. Fails the test
/// when the source parses with an error or a diagnostic, or when neither node is at the position.
let private classifyLineStartOperator (source: string) (line: int) : LineStartOperator =
    let lexed = XParsec.FSharp.Lexer.Lexing.lexString source
    let reader = XParsec.FSharp.Parser.Reader.ofParseInput (lexed.WithDefines Set.empty)

    match XParsec.FSharp.Parser.FSharpAst.parse reader with
    | Error e -> failtestf "Parsing failed:\n%s" (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
    | Ok ast ->
        Expect.isEmpty reader.State.Diagnostics "the source parses without diagnostics"
        let lines = source.Split '\n'
        let lineStart = lines[.. line - 2] |> Array.sumBy (fun l -> l.Length + 1)

        let offset =
            lineStart + (lines[line - 1].Length - lines[line - 1].TrimStart().Length)

        let ctx = XParsec.FSharp.Debug.PrintContext(2)
        XParsec.FSharp.Debug.printFSharpAst ctx lexed ast
        let printed = ctx.FlushToString().Split '\n' |> Array.map _.TrimEnd()

        let hasRow (nodes: string) =
            let row = Text.RegularExpressions.Regex($@"^\s*({nodes})\s.*\bpos=\s*{offset}$")
            printed |> Array.exists row.IsMatch

        match hasRow "InfixApp|Assignment", hasRow "PrefixApp" with
        | true, false -> Infix
        | false, true -> NewExpression
        | _ -> failtestf "Expected one operator node at offset %d in:\n%s" offset (String.concat "\n" printed)

/// fsc's offside rule (LexFilter `isInfix`) for an operator at a sequential block's column: an
/// infix operator continues the expression, and a `+`/`-`-led operator or `&&` is infix unless it
/// is adjacent to its operand. Each expectation is the fsi result.
[<Tests>]
let lineStartOperatorTests =
    let case name (expected: LineStartOperator) (line: int) (source: string) =
        test name { Expect.equal (classifyLineStartOperator source line) expected "the operator's role" }

    testList
        "LineStartOperators"
        [
            case
                "+ at the column continues a let body"
                Infix
                5
                "let a = \"x\"\nlet b = \"y\"\nlet s =\n    a + \" \"\n    + b\n"
            case "- at the column continues a let body" Infix 3 "let s =\n    5\n    - 2\n"
            case
                "- x at the column continues the last expression of a block"
                Infix
                4
                "let f () =\n    let x = 3\n    x\n    - x\n"
            case "a +-led custom operator at the column continues" Infix 3 "let s =\n    1\n    +. 2\n"
            case "+ at the column continues inside parentheses" Infix 3 "let s =\n    (a\n     + b)\n"
            case
                "+ at the column continues the body of a nested let"
                Infix
                4
                "let f () =\n    let y =\n        1\n        + 2\n    y\n"
            case "- at the column continues a list element" Infix 3 "let s =\n    [ a\n      - b ]\n"
            case "|> at the column continues" Infix 3 "let s =\n    xs\n    |> List.map f\n"
            case "&& at the column continues" Infix 3 "let s =\n    true\n    && false\n"
            case ":: at the column continues" Infix 3 "let s =\n    1\n    :: [ 2 ]\n"
            case ":= at the column continues" Infix 3 "let f () =\n    r\n    := 5\n"
            case ">>= at the column continues" Infix 3 "let s =\n    1\n    >>= f\n"
            case "+ one column left of the column continues" Infix 3 "let s =\n    a\n   + b\n"
            case "|> three columns left of the column continues" Infix 3 "let s =\n    xs\n |> f\n"
            case
                "an adjacent -x at the column starts a new expression"
                NewExpression
                4
                "let f () =\n    let x = 3\n    x\n    -x\n"
            case
                "an adjacent &&x at the column starts a new expression"
                NewExpression
                3
                "let s =\n    true\n    &&false\n"
            case "an adjacent -b in a list starts a new element" NewExpression 3 "let s =\n    [ a\n      -b ]\n"
            case "% at the column starts a new expression" NewExpression 3 "let s =\n    7\n    % 2\n"
            // fsi rejects this source with FS0010 at `-x`; Vesper reads `-x` as a top-level expression.
            case
                "an adjacent -x one column left of the column is outside the infix undentation"
                NewExpression
                4
                "let x = 3\nlet s =\n    x\n   -x\n"
        ]

/// Tests that parsing every prefix of every golden file at token boundaries always returns Ok.
/// This validates the error recovery: no truncation should cause a parse Error or exception.
let testSlicedParsing (filePath: string) =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")

    // Get token boundaries from the full lex
    let lexed = XParsec.FSharp.Lexer.Lexing.lexString input

    // Collect unique character positions at token boundaries (StartIndex of each token)
    let boundaries =
        [|
            yield 0 // empty input
            for i in 0 .. lexed.Tokens.Length - 1 do
                let tok = lexed.Tokens.[i * 1<XParsec.FSharp.Lexer.token>]
                let startIdx = tok.StartIndex

                if startIdx > 0 && startIdx <= input.Length then
                    yield startIdx
        |]
        |> Array.distinct
        |> Array.sort

    let mutable failures = ResizeArray<string>()

    for boundary in boundaries do
        let slice = input.[.. boundary - 1] // Take first `boundary` characters

        let reader =
            XParsec.FSharp.Parser.Reader.ofParseInput (
                (XParsec.FSharp.Lexer.Lexing.lexString slice).WithDefines Set.empty
            )

        try
            match XParsec.FSharp.Parser.FSharpAst.parse reader with
            | Error e ->
                failures.Add(
                    $"  length={boundary}: Error - {XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e}"
                )
            | Ok _ -> ()
        with ex ->
            failures.Add($"  length={boundary}: Exception - {ex.GetType().Name}: {ex.Message}")

    if failures.Count > 0 then
        let details = String.Join("\n", failures)

        failtestf
            "Sliced parsing failed for %d of %d boundaries in %s:\n%s"
            failures.Count
            boundaries.Length
            (Path.GetFileName filePath)
            details

[<Tests>]
let recoveryTests =
    testList
        "RecoveryTests"
        [
            for path in testData.Value do
                let fileName = Path.GetFileName path
                let name = $"Sliced recovery {fileName}"
                test name { testSlicedParsing path }
        ]
