namespace XParsec.FSharp.Parser

open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.Parser.ParseState

[<RequireQualifiedAccess>]
module ImplementationFile =
    /// Parses a named module: `[attributes] module [access] LongIdent <module-elems>`.
    /// Distinguished from a module abbreviation (`module X = Y.Z`) by the absence of `=` after the identifier.
    let private pNamedModule =
        let notFollowedByEquals = notFollowedBySyntaxToken Token.OpEquality
        do ObjectConstruction.init () // Initialize refObjectConstruction via static constructor

        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            // Once 'module' is matched, recover missing/malformed identifiers with a virtual token.
            let! longIdent = recoverLongIdent "Expected module identifier" LongIdent.parse
            // Avoid matching module abbreviations (e.g., `module M = Other.Module`).
            do! notFollowedByEquals
            let! elems = ModuleElem.parseElemsWithRecovery
            return NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems)
        }

    let parse =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWNamespace, many1 NamespaceDeclGroup.parse |>> ImplementationFile.Namespaces
                Token.KWModule,
                // Match named module first; fall back to anonymous module (e.g. file begins with a module abbreviation).
                choiceL
                    [
                        pNamedModule |>> ImplementationFile.NamedModule
                        ModuleElem.parseElems |>> ImplementationFile.AnonymousModule
                    ]
                    "ImplementationFile"
            ]
            // File does not start with `namespace` or `module` (e.g., attributes or top-level declarations).
            (choiceL
                [
                    pNamedModule |>> ImplementationFile.NamedModule
                    ModuleElem.parseElems |>> ImplementationFile.AnonymousModule
                ]
                "ImplementationFile")

[<RequireQualifiedAccess>]
module FSharpAst =
    let private initialization =
        lazy
            (Measure.init ()
             ObjectConstruction.init ())

    /// Consumes the EOF token (including trailing trivia/directives) and asserts end of input.
    let private pEof = nextSyntaxTokenIsLMsg Token.EOF "Expected end of file" .>> eof

    // Plain `choice` is used here so individual parser error messages are preserved
    // rather than masked by a labeled `choiceL`.
    let private pNormal =
        choice
            [
                ImplementationFile.parse .>> pEof |>> FSharpAst.ImplementationFile
                Expr.parse .>> pEof
                |>> (fun e ->
                    FSharpAst.ScriptFragment(
                        ScriptFragment.ScriptFragment(ImmutableArray.Create(ModuleElem.Expression e))
                    )
                )
            ]

    /// Consumes and collects all remaining non-EOF tokens into an array.
    let private skipToEof (reader: Reader<PositionedToken, ParseState, _>) =
        let skipped = ResizeArray<SyntaxToken>()
        let mutable keepGoing = true

        while keepGoing do
            match peekNextSyntaxToken reader with
            | Error _ -> keepGoing <- false
            | Ok tok ->
                if tok.Token = Token.EOF then
                    keepGoing <- false
                else
                    match consumePeeked tok reader with
                    | Ok t -> skipped.Add(t)
                    | Error _ -> keepGoing <- false

        ImmutableArray.CreateRange(skipped)

    /// Appends unparsed tokens as a `SkipsTokens` element to the AST container for diagnostic reporting.
    let private appendSkippedToImpl (skipped: ImmutableArray<SyntaxToken>) (implFile: ImplementationFile<SyntaxToken>) =
        let skipElem = ModuleElem.SkipsTokens(skipped)

        match implFile with
        | ImplementationFile.AnonymousModule elems -> ImplementationFile.AnonymousModule(elems.Add(skipElem))
        | ImplementationFile.NamedModule(NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems)) ->
            ImplementationFile.NamedModule(
                NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems.Add(skipElem))
            )
        | ImplementationFile.Namespaces decls when decls.Length > 0 ->
            let lastIdx = decls.Length - 1

            let updatedLast =
                match decls[lastIdx] with
                | NamespaceDeclGroup.Named(ns, isRec, ident, elems) ->
                    NamespaceDeclGroup.Named(ns, isRec, ident, elems.Add(skipElem))
                | NamespaceDeclGroup.Global(ns, g, elems) -> NamespaceDeclGroup.Global(ns, g, elems.Add(skipElem))

            ImplementationFile.Namespaces(decls.SetItem(lastIdx, updatedLast))
        | ImplementationFile.Namespaces _ -> ImplementationFile.AnonymousModule(ImmutableArray.Create(skipElem))

    /// Appends unparsed tokens as a `SkipsTokens` element to the signature AST container for diagnostic reporting.
    let private appendSkippedToSig (skipped: ImmutableArray<SyntaxToken>) (sigFile: SignatureFile<SyntaxToken>) =
        let skipElem = ModuleSignatureElement.SkipsTokens(skipped)

        match sigFile with
        | SignatureFile.AnonymousModule elems -> SignatureFile.AnonymousModule(elems.Add(skipElem))
        | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(attrs,
                                                                              modTok,
                                                                              access,
                                                                              isRec,
                                                                              longIdent,
                                                                              elems)) ->
            SignatureFile.NamedModule(
                NamedModuleSignature.NamedModuleSignature(attrs, modTok, access, isRec, longIdent, elems.Add(skipElem))
            )
        | SignatureFile.Namespaces decls when decls.Length > 0 ->
            let lastIdx = decls.Length - 1

            let updatedLast =
                match decls[lastIdx] with
                | NamespaceDeclGroupSignature.Named(ns, isRec, ident, elems) ->
                    NamespaceDeclGroupSignature.Named(ns, isRec, ident, elems.Add(skipElem))
                | NamespaceDeclGroupSignature.Global(ns, g, elems) ->
                    NamespaceDeclGroupSignature.Global(ns, g, elems.Add(skipElem))

            SignatureFile.Namespaces(decls.SetItem(lastIdx, updatedLast))
        | SignatureFile.Namespaces _ -> SignatureFile.AnonymousModule(ImmutableArray.Create(skipElem))

    /// Records a block comment left open at the end of input as a diagnostic at its outermost `(*`.
    let private reportUnclosedComment (reader: Reader<PositionedToken, ParseState, _>) =
        let lexed = reader.State.Lexed

        match lexed.UnclosedComment with
        | ValueNone -> ()
        | ValueSome unclosed ->
            let token =
                SyntaxToken.syntaxToken lexed.Tokens[unclosed.Opener] (int unclosed.Opener)

            reader.State <- addDiagnosticAt (DiagnosticCode.UnterminatedComment unclosed.Ending) token reader.State

    let private parseSignatureFile: Parser<FSharpAst<SyntaxToken>, PositionedToken, ParseState, _> =
        fun reader ->
            initialization.Force()
            let startPos = reader.Position

            let fileIndent =
                match peekNextSyntaxToken reader with
                | Ok tok ->
                    match tok.Index with
                    | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                    | TokenIndex.Virtual -> 0
                | Error _ -> 0

            let entry: Offside =
                {
                    Context = OffsideContext.SeqBlock
                    // Uses a synthetic EOF token so `Offside.isDeclBlock` can identify the root file-level frame.
                    Indent = fileIndent
                    Token = PositionedToken.Create(Token.EOF, 0)
                }

            reader.State <- ParseState.pushOffside entry reader.State

            let pSig = SignatureFile.parse .>> pEof |>> FSharpAst.SignatureFile

            match pSig reader with
            | Ok result -> Ok result
            | Error topErr ->
                // Full parse failed: attempt to parse as much as possible without requiring EOF, then capture trailing tokens.
                reader.Position <- startPos

                match SignatureFile.parse reader with
                | Ok sigFile ->
                    let skipped = skipToEof reader

                    let sigFile' =
                        if skipped.IsEmpty then
                            sigFile
                        else
                            let startTok = skipped.[0]

                            reader.State <-
                                addDiagnosticWithError DiagnosticCode.UnexpectedTopLevel startTok topErr reader.State

                            appendSkippedToSig skipped sigFile

                    Ok(FSharpAst.SignatureFile sigFile')

                | Error _ ->
                    // Total failure: return an empty signature with all tokens captured as skipped.
                    reader.Position <- startPos
                    let skipped = skipToEof reader

                    let elems =
                        if not skipped.IsEmpty then
                            let startTok = skipped.[0]

                            reader.State <-
                                addDiagnosticWithError DiagnosticCode.UnexpectedTopLevel startTok topErr reader.State

                            ImmutableArray.Create(ModuleSignatureElement.SkipsTokens(skipped))
                        else
                            ImmutableArray.Empty

                    Ok(FSharpAst.SignatureFile(SignatureFile.AnonymousModule elems))

    /// Top-level parser for signature files (`.fsi`).
    /// Infallible: always returns `Ok`, recording parse errors as diagnostics and unparsed tokens as `SkipsTokens`.
    let parseSignature: Parser<FSharpAst<SyntaxToken>, PositionedToken, ParseState, _> =
        fun reader ->
            let result = parseSignatureFile reader
            reportUnclosedComment reader
            result

    let private parseImplementationFile: Parser<FSharpAst<SyntaxToken>, PositionedToken, ParseState, _> =
        fun reader ->
            initialization.Force()
            let startPos = reader.Position

            // Establish a base `SeqBlock` context so undentation rules have a root offside line.
            let fileIndent =
                match peekNextSyntaxToken reader with
                | Ok tok ->
                    match tok.Index with
                    | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                    | TokenIndex.Virtual -> 0
                | Error _ -> 0

            let entry: Offside =
                {
                    Context = OffsideContext.SeqBlock
                    // Uses a synthetic EOF token so `Offside.isDeclBlock` can identify the root file-level frame.
                    Indent = fileIndent
                    Token = PositionedToken.Create(Token.EOF, 0)
                }

            reader.State <- ParseState.pushOffside entry reader.State

            match pNormal reader with
            | Ok result -> Ok result
            | Error topErr ->
                // 1. Fallback: Parse an implementation file without requiring EOF, capturing trailing tokens.
                reader.Position <- startPos

                match ImplementationFile.parse reader with
                | Ok implFile ->
                    let skipped = skipToEof reader

                    let implFile' =
                        if skipped.IsEmpty then
                            implFile
                        else
                            let startTok = skipped.[0]

                            reader.State <-
                                addDiagnosticWithError DiagnosticCode.UnexpectedTopLevel startTok topErr reader.State

                            appendSkippedToImpl skipped implFile

                    Ok(FSharpAst.ImplementationFile implFile')

                | Error _ ->
                    // 2. Fallback: Parse as a standalone script expression.
                    reader.Position <- startPos

                    match Expr.parse reader with
                    | Ok expr ->
                        let skipped = skipToEof reader
                        let elems = ImmutableArray.CreateBuilder(if skipped.IsEmpty then 1 else 2)
                        elems.Add(ModuleElem.Expression expr)

                        if not skipped.IsEmpty then
                            let startTok = skipped.[0]

                            reader.State <-
                                addDiagnosticWithError DiagnosticCode.UnexpectedTopLevel startTok topErr reader.State

                            elems.Add(ModuleElem.SkipsTokens(skipped))

                        Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment(elems.ToImmutable())))

                    | Error _ ->
                        // 3. Fallback: Return an empty module with all tokens captured as skipped.
                        reader.Position <- startPos
                        let skipped = skipToEof reader

                        let elems =
                            if not skipped.IsEmpty then
                                let startTok = skipped.[0]

                                reader.State <-
                                    addDiagnosticWithError
                                        DiagnosticCode.UnexpectedTopLevel
                                        startTok
                                        topErr
                                        reader.State

                                ImmutableArray.Create(ModuleElem.SkipsTokens(skipped))
                            else
                                ImmutableArray.Empty

                        Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elems))

    /// Top-level parser for implementation files (`.fs`).
    /// Infallible: always returns `Ok`, recording parse errors as diagnostics and unparsed tokens as `SkipsTokens`.
    let parse: Parser<FSharpAst<SyntaxToken>, PositionedToken, ParseState, _> =
        fun reader ->
            let result = parseImplementationFile reader
            reportUnclosedComment reader
            result
