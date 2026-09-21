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

    member _.IndentSize = indentSize

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

/// Prints a section header and runs `f` indented under it.
let printSection (ctx: PrintContext) (header: string) (f: unit -> unit) =
    ctx.WriteLine($"{header}:")
    indent ctx f

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
let makeDebugVisitor (ctx: PrintContext) (lexed: Lexed) : AstVisitor<SyntaxToken> =
    {
        VisitToken = fun label tok -> printTokenRow label ctx lexed tok
        EnterSection =
            fun name ->
                if name <> "" then
                    ctx.WriteLine($"{name}:")

                ctx.Indent <- ctx.Indent + 1
        ExitSection = fun _ -> ctx.Indent <- ctx.Indent - 1
        WriteLine = fun text -> ctx.WriteLine text
        EqualTokens = fun a b -> a.Index = b.Index
    }

// ---- Print functions (wrappers over AstTraversal walk functions) ----

let printAccess (ctx: PrintContext) (lexed: Lexed) (access: Access<SyntaxToken>) =
    walkAccess (makeDebugVisitor ctx lexed) access

let printConstant (label: string) (ctx: PrintContext) (lexed: Lexed) (x: Constant<SyntaxToken>) =
    walkConstant (makeDebugVisitor ctx lexed) label x

let printIdentOrOp (ctx: PrintContext) (lexed: Lexed) (identOrOp: IdentOrOp<SyntaxToken>) =
    walkIdentOrOp (makeDebugVisitor ctx lexed) identOrOp

let printOpName (ctx: PrintContext) (lexed: Lexed) (opName: OpName<SyntaxToken>) =
    walkOpName (makeDebugVisitor ctx lexed) opName

let printRangeOpName (ctx: PrintContext) (lexed: Lexed) (rangeOpName: RangeOpName<SyntaxToken>) =
    walkRangeOpName (makeDebugVisitor ctx lexed) rangeOpName

let printActivePatternOpName
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (activePatternOpName: ActivePatternOpName<SyntaxToken>)
    =
    walkActivePatternOpName (makeDebugVisitor ctx lexed) activePatternOpName

let printLongIdentOrOp (ctx: PrintContext) (lexed: Lexed) (longIdentOrOp: LongIdentOrOp<SyntaxToken>) =
    walkLongIdentOrOp (makeDebugVisitor ctx lexed) longIdentOrOp

let printPat (ctx: PrintContext) (lexed: Lexed) (pat: Pat<SyntaxToken>) =
    walkPat (makeDebugVisitor ctx lexed) pat

let printTypar (ctx: PrintContext) (lexed: Lexed) (typar: Typar<SyntaxToken>) =
    walkTypar (makeDebugVisitor ctx lexed) typar

let printConstraint (ctx: PrintContext) (lexed: Lexed) (c: Constraint<SyntaxToken>) =
    walkConstraint (makeDebugVisitor ctx lexed) c

let printTyparDefns (ctx: PrintContext) (lexed: Lexed) (typars: TyparDefns<SyntaxToken>) =
    walkTyparDefns (makeDebugVisitor ctx lexed) typars

let printTypeArg (ctx: PrintContext) (lexed: Lexed) (typeArg: TypeArg<SyntaxToken>) =
    walkTypeArg (makeDebugVisitor ctx lexed) typeArg

let printMeasure (ctx: PrintContext) (lexed: Lexed) (measure: Measure<SyntaxToken>) =
    walkMeasure (makeDebugVisitor ctx lexed) measure

let printType (ctx: PrintContext) (lexed: Lexed) (ty: Type<SyntaxToken>) =
    walkType (makeDebugVisitor ctx lexed) ty

let printBinding (ctx: PrintContext) (lexed: Lexed) (binding: Binding<SyntaxToken>) =
    walkBinding (makeDebugVisitor ctx lexed) binding

let printFieldInitializer
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (fieldInit: FieldInitializer<SyntaxToken>)
    =
    walkFieldInitializer (makeDebugVisitor ctx lexed) fieldInit

let printRules (ctx: PrintContext) (lexed: Lexed) (rules: Rules<SyntaxToken>) =
    walkRules (makeDebugVisitor ctx lexed) rules

let printExpr (ctx: PrintContext) (lexed: Lexed) (expr: Expr<SyntaxToken>) =
    walkExpr (makeDebugVisitor ctx lexed) expr

let printArgSpec (ctx: PrintContext) (lexed: Lexed) (argSpec: ArgSpec<SyntaxToken>) =
    walkArgSpec (makeDebugVisitor ctx lexed) argSpec

let printUncurriedSig (ctx: PrintContext) (lexed: Lexed) (sign: UncurriedSig<SyntaxToken>) =
    walkUncurriedSig (makeDebugVisitor ctx lexed) sign

let printCurriedSig (ctx: PrintContext) (lexed: Lexed) (sign: CurriedSig<SyntaxToken>) =
    walkCurriedSig (makeDebugVisitor ctx lexed) sign

let printMemberSig (ctx: PrintContext) (lexed: Lexed) (sign: MemberSig<SyntaxToken>) =
    walkMemberSig (makeDebugVisitor ctx lexed) sign

let printMethodOrPropDefn (ctx: PrintContext) (lexed: Lexed) (defn: MethodOrPropDefn<SyntaxToken>) =
    walkMethodOrPropDefn (makeDebugVisitor ctx lexed) defn

let printMemberDefn (ctx: PrintContext) (lexed: Lexed) (memberDefn: MemberDefn<SyntaxToken>) =
    walkMemberDefn (makeDebugVisitor ctx lexed) memberDefn

let printTypeDefnElement (ctx: PrintContext) (lexed: Lexed) (elem: TypeDefnElement<SyntaxToken>) =
    walkTypeDefnElement (makeDebugVisitor ctx lexed) elem

let printTypeName (ctx: PrintContext) (lexed: Lexed) (typeName: TypeName<SyntaxToken>) =
    walkTypeName (makeDebugVisitor ctx lexed) typeName

let printPrimaryConstrArgs (ctx: PrintContext) (lexed: Lexed) (args: PrimaryConstrArgs<SyntaxToken>) =
    walkPrimaryConstrArgs (makeDebugVisitor ctx lexed) args

let printUnionCaseData (ctx: PrintContext) (lexed: Lexed) (data: UnionTypeCaseData<SyntaxToken>) =
    walkUnionCaseData (makeDebugVisitor ctx lexed) data

let printExceptionDefn (ctx: PrintContext) (lexed: Lexed) (exnDefn: ExceptionDefn<SyntaxToken>) =
    walkExceptionDefn (makeDebugVisitor ctx lexed) exnDefn

let printTypeDefn (ctx: PrintContext) (lexed: Lexed) (typeDefn: TypeDefn<SyntaxToken>) =
    walkTypeDefn (makeDebugVisitor ctx lexed) typeDefn

let printModuleFunctionOrValueDefn
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (defn: ModuleFunctionOrValueDefn<SyntaxToken>)
    =
    walkModuleFunctionOrValueDefn (makeDebugVisitor ctx lexed) defn

let printImportDecl (ctx: PrintContext) (lexed: Lexed) (decl: ImportDecl<SyntaxToken>) =
    walkImportDecl (makeDebugVisitor ctx lexed) decl

let printModuleAbbrev (ctx: PrintContext) (lexed: Lexed) (abbrev: ModuleAbbrev<SyntaxToken>) =
    walkModuleAbbrev (makeDebugVisitor ctx lexed) abbrev

let printCompilerDirective
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (decl: CompilerDirectiveDecl<SyntaxToken>)
    =
    walkCompilerDirective (makeDebugVisitor ctx lexed) decl

let printModuleElem (ctx: PrintContext) (lexed: Lexed) (elem: ModuleElem<SyntaxToken>) =
    walkModuleElem (makeDebugVisitor ctx lexed) elem

let printModuleDefn (ctx: PrintContext) (lexed: Lexed) (defn: ModuleDefn<SyntaxToken>) =
    walkModuleDefn (makeDebugVisitor ctx lexed) defn

let printModuleElems (ctx: PrintContext) (lexed: Lexed) (elems: ModuleElems<SyntaxToken>) =
    walkModuleElems (makeDebugVisitor ctx lexed) elems

let printNamespaceDeclGroup
    (ctx: PrintContext)
    (input: string)
    (lexed: Lexed)
    (group: NamespaceDeclGroup<SyntaxToken>)
    =
    walkNamespaceDeclGroup (makeDebugVisitor ctx lexed) group

let printImplementationFile (ctx: PrintContext) (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) =
    walkImplementationFile (makeDebugVisitor ctx lexed) file

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
