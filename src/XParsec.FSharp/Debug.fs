module XParsec.FSharp.Debug

open System
open System.IO

open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.AstTraversal

// ---- Buffered output types ----

[<Struct>]
type private TokenRowData =
    {
        IndentLevel: int
        Label: string
        QuotedLit: string
        Kind: string
        TokIdx: string
        CharPos: string
    }

[<RequireQualifiedAccess>]
type private OutputItem =
    | Line of indentLevel: int * text: string
    | TokenRow of TokenRowData

/// Buffers text and token rows, then flushes them with dynamically aligned columns
/// across all indentation levels.
type PrintContext(indentSize: int) =
    let items = ResizeArray<OutputItem>()
    let mutable currentIndent = 0

    member _.Indent
        with get () = currentIndent
        and set v = currentIndent <- v

    member ctx.WriteLine(text: string) =
        items.Add(OutputItem.Line(ctx.Indent, text))

    member ctx.AddTokenRow(label: string, quotedLit: string, kind: string, tokIdx: string, charPos: string) =
        items.Add(
            OutputItem.TokenRow
                {
                    IndentLevel = ctx.Indent
                    Label = label
                    QuotedLit = quotedLit
                    Kind = kind
                    TokIdx = tokIdx
                    CharPos = charPos
                }
        )

    member _.Flush(writer: TextWriter) =
        // Calculate max width for the label column (indentation + label length).
        let maxLabelColWidth =
            let mutable m = 0

            for item in items do
                match item with
                | OutputItem.TokenRow row ->
                    let w = row.IndentLevel * indentSize + row.Label.Length

                    if w > m then
                        m <- w
                | _ -> ()

            m

        for item in items do
            match item with
            | OutputItem.Line(ind, text) -> writer.WriteLine(String.replicate (ind * indentSize) " " + text)
            | OutputItem.TokenRow row ->
                let indentStr = String.replicate (row.IndentLevel * indentSize) " "
                let labelPadded = (indentStr + row.Label).PadRight(maxLabelColWidth)

                writer.WriteLine(
                    $"{labelPadded}  {row.QuotedLit, -16}  {row.Kind, -18}  {row.TokIdx, -8}  {row.CharPos}"
                )

    member ctx.FlushToString() =
        use sw = new StringWriter()
        ctx.Flush(sw)
        sw.ToString()

// ---- Helpers ----

/// Increases indentation by one level for the duration of `f`.
let inline indent (ctx: PrintContext) (f: unit -> unit) =
    ctx.Indent <- ctx.Indent + 1
    f ()
    ctx.Indent <- ctx.Indent - 1

let tokenKindString (lexed: Lexed) (token: PositionedToken) (i: int<token>) =
    let rawToken = lexed.Tokens.[i]

    // For split tokens (e.g. '>\]' split into '>' and '\]'), use the rewritten token kind
    // directly instead of deriving an operator name from the original source text.
    if rawToken.Token <> token.Token || rawToken.StartIndex <> token.StartIndex then
        $"{TokenInfo.withoutFlags token.Token}"
    else
        let s = lexed.GetTokenString(i)

        match OperatorInfo.TryCreate token with
        | ValueSome opInfo -> opInfo.GetName(s)
        | ValueNone -> $"{TokenInfo.withoutFlags rawToken.Token}"

/// Adds a single formatted token row to the print buffer.
let printTokenRow (label: string) (ctx: PrintContext) (lexed: Lexed) (token: SyntaxToken) =
    let pt = token.PositionedToken

    match token.Index with
    | TokenIndex.Virtual ->
        let kind = $"{TokenInfo.withoutFlags pt.Token}"
        let charPos = pt.StartIndex
        ctx.AddTokenRow(label, "<virt>", kind, "tok=virt", $"pos=%4i{charPos}")
    | TokenIndex.Regular iT ->
        let t1 = lexed.Tokens.[iT + 1<_>]
        let charPos = pt.StartIndex
        let charEnd = t1.StartIndex - 1
        let len = charEnd - charPos + 1

        let lit =
            let rawLit =
                if len > 10 then
                    lexed.Input.[charPos .. charPos + 9] + "..."
                else
                    lexed.Input.[charPos..charEnd]

            rawLit.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

        let kind = tokenKindString lexed pt iT
        ctx.AddTokenRow(label, $"'{lit}'", kind, $"tok=%4i{int iT}", $"pos=%4i{charPos}")

// ---- Visitor factory ----

/// Creates an AST visitor that writes formatted debug output to the given context.
let makeDebugVisitor (ctx: PrintContext) (lexed: Lexed) : IAstVisitor<SyntaxToken> =
    { new IAstVisitor<SyntaxToken> with
        member _.VisitToken label tok = printTokenRow label ctx lexed tok

        member _.EnterSection name =
            if name <> "" then
                ctx.WriteLine($"{name}:")

            ctx.Indent <- ctx.Indent + 1

        member _.ExitSection _ = ctx.Indent <- ctx.Indent - 1
        member _.WriteLine text = ctx.WriteLine text
        member _.Missing label = ctx.WriteLine label
        member _.Skipped tok = printTokenRow "(skipped)" ctx lexed tok
    }

// ---- Print functions (wrappers over AstTraversal walk functions) ----

let printFSharpAst (ctx: PrintContext) (lexed: Lexed) (ast: FSharpAst<SyntaxToken>) =
    walkFSharpAst (makeDebugVisitor ctx lexed) ast

/// Formats a diagnostic code and any relevant payload data for golden testing.
///
/// Note: This match is deliberately exhaustive without a wildcard pattern so that
/// any newly added diagnostic codes or payloads must be explicitly accounted for.
let sprintDiagnosticCode (code: DiagnosticCode) : string =
    let name = DiagnosticCode.code code

    match code with
    | DiagnosticCode.Other msg -> $"{name}({msg})"
    | DiagnosticCode.UnclosedDelimiter(opened = opened; expected = expected)
    | DiagnosticCode.MismatchedDelimiter(opened = opened; expected = expected) ->
        let openedBase = TokenInfo.withoutFlags opened
        let expectedBase = TokenInfo.withoutFlags expected
        $"{name}({openedBase}, {expectedBase})"
    | DiagnosticCode.TyparInConstant
    | DiagnosticCode.MissingExpression
    | DiagnosticCode.MissingPattern
    | DiagnosticCode.MissingType
    | DiagnosticCode.MissingRule
    | DiagnosticCode.MissingTypeDefn
    | DiagnosticCode.MissingModuleElem
    | DiagnosticCode.UnexpectedTopLevel
    | DiagnosticCode.ExpectedEnd
    | DiagnosticCode.ExpectedRParen
    | DiagnosticCode.ExpectedRBracket
    | DiagnosticCode.ExpectedRArrayBracket
    | DiagnosticCode.ExpectedRBraceBar
    | DiagnosticCode.ExpectedQuotationTypedRight
    | DiagnosticCode.ExpectedQuotationUntypedRight -> name
    | DiagnosticCode.UnterminatedComment ending -> $"{name}({ending})"

/// Prints diagnostics in source order if any are present.
let printDiagnostics (ctx: PrintContext) (input: string) (diagnostics: Diagnostic list) =
    if not diagnostics.IsEmpty then
        let lineIndex = LineIndex.OfString input

        ctx.WriteLine("---")
        ctx.WriteLine("Diagnostics:")

        indent
            ctx
            (fun () ->
                for diag in List.rev diagnostics do
                    let pos = diag.Token.StartIndex
                    let struct (ln, col) = lineIndex.GetLineCol(min pos (input.Length))
                    ctx.WriteLine($"{sprintDiagnosticCode diag.Code} at {pos} (Ln {ln}, Col {col})")

                    match diag.Error with
                    | Some err ->
                        indent
                            ctx
                            (fun () ->
                                let formatted = ErrorFormatting.splitAndFormatTokenErrors err

                                for line in formatted.Split('\n') do
                                    ctx.WriteLine(line.TrimEnd())
                            )
                    | None -> ()
            )

/// Prints compiler warning directives (#nowarn / #warnon) in source order if any are present.
let printWarnDirectives (ctx: PrintContext) (warnDirectives: WarnDirective list) =
    if not warnDirectives.IsEmpty then
        ctx.WriteLine("---")
        ctx.WriteLine("WarnDirectives:")

        indent
            ctx
            (fun () ->
                for d in List.rev warnDirectives do
                    let kind = if d.Suppress then "#nowarn" else "#warnon"
                    ctx.WriteLine($"{kind} {d.WarningNumber} (Ln {d.Line})")
            )
