namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState

[<RequireQualifiedAccess>]
module ImportDecl =
    let parse =
        parser {
            let! openTok = pOpen
            let! typeTok = opt pType

            // Recover with a virtual identifier if LongIdent fails after 'open'
            let! ident = recoverLongIdent "Expected identifier after 'open'" LongIdent.parse

            match typeTok with
            | ValueSome typeTok -> return ImportDeclType(openTok, typeTok, ident)
            | ValueNone -> return ImportDecl(openTok, ident)
        }

[<RequireQualifiedAccess>]
module ModuleAbbrev =
    let parse =
        parser {
            let! modTok = pModule
            let! ident = nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "Expected identifier"
            let! eq = pEquals
            let! lid = LongIdent.parse
            return ModuleAbbrev.ModuleAbbrev(modTok, ident, eq, lid)
        }

[<RequireQualifiedAccess>]
module CompilerDirectiveDecl =
    let parse: FSParser<CompilerDirectiveDecl<SyntaxToken>> =
        parser {
            let! hash = pHash
            // Recover with a virtual identifier if missing after '#'
            let! ident =
                recoverWithVirtualToken
                    Token.Identifier
                    "Expected directive identifier after '#'"
                    (nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "Directive identifier")

            let! strings = many (nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token.IsText) "String argument")
            return CompilerDirectiveDecl(hash, ident, strings)
        }

[<RequireQualifiedAccess>]
module ModuleFunctionOrValueDefn =

    let private errTrailingInNoBody: ErrorType<PositionedToken, ParseState> =
        Message "trailing 'in' at end of file has no body"

    let private errExpectedLetOrDo: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'let' or 'do'"

    // Only consume trailing 'in' if followed by another token.
    // If at EOF, fail so the missing-body diagnostic handles it downstream.
    let private pTrailingIn =
        parser {
            let! inTok = pIn

            match! peekNextSyntaxToken with
            | t when t.Token <> Token.EOF -> return inTok
            | _ -> return! fail errTrailingInNoBody
        }

    let private pLetBinding attrs letTok =
        parser {
            let! isRec = opt pRec

            let! recTok, bindings, ands =
                match isRec with
                | ValueSome recTok ->
                    parser {
                        let! bindings, ands = Binding.parseSepByAnd1 attrs
                        return ValueSome recTok, bindings, ands
                    }
                | ValueNone ->
                    parser {
                        let! binding = Binding.parse attrs
                        return ValueNone, ImmutableArray.Create(binding), ImmutableArray.Empty
                    }

            // Capture optional ML-style trailing 'in' to preserve it in the AST
            let! inTok = opt pTrailingIn
            return ModuleFunctionOrValueDefn.Let(attrs, letTok, recTok, bindings, ands, inTok)
        }

    let parse =
        parser {
            let! attrs = opt Attributes.parse

            let! token = nextSyntaxToken

            match token.Token with
            | Token.KWDo ->
                // Recover with Expr.Missing if the 'do' body expression fails
                let! expr =
                    recoverWith
                        StoppingTokens.afterExpr
                        DiagnosticCode.MissingExpression
                        (missingOrSkipped Expr.Missing Expr.SkipsTokens)
                        Expr.pTypedSeqExprBlock

                let! inTok = opt pTrailingIn
                return ModuleFunctionOrValueDefn.Do(attrs, token, expr, inTok)

            | Token.KWLet -> return! pLetBinding attrs token

            | _ -> return! fail errExpectedLetOrDo
        }

[<RequireQualifiedAccess>]
module ModuleDefn =

    let parseBody
        (modTok: SyntaxToken)
        (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _>)
        : Parser<_, _, _, _> =
        parser {
            let! state = getUserState

            let indent =
                match modTok.Index with
                | TokenIndex.Regular iT -> ParseState.getIndent state iT
                | TokenIndex.Virtual -> 0

            let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin
            // Indent module body 1 column past the 'module' keyword
            let! elems = withContextAt OffsideContext.Module (indent + 1) modTok.PositionedToken (opt elementParser)
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return ModuleDefnBody(beginTok, elems, endTok)
        }

    let parse (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _>) =
        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            // Recover missing identifier and '=' with virtual tokens after 'module' keyword
            let! ident =
                recoverWithVirtualToken
                    Token.Identifier
                    "Expected module identifier"
                    (nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "Module identifier")

            let! eq = recoverWithVirtualToken Token.OpEquality "Expected '=' after module identifier" pEquals

            let! body = parseBody modTok elementParser
            return ModuleDefn(attrs, modTok, access, isRec, ident, eq, body)
        }

[<RequireQualifiedAccess>]
module ModuleElem =
    // Forward reference for mutual recursion: ModuleElem -> ModuleDefn -> ModuleElem
    let private refModuleElem = RefParser<ModuleElem<SyntaxToken>, _, _, _>()

    /// Parses module elements without error recovery. Used where backtracking is required (e.g. anonymous modules).
    let parseElems = many refModuleElem.Parser

    /// Parses module elements with error recovery. On failure, skips tokens until the next element boundary and continues.
    /// Safe only in committed contexts (e.g., inside an explicit module or namespace).
    let parseElemsWithRecovery: Parser<ModuleElems<SyntaxToken>, _, _, _> =
        fun reader ->
            let result = ResizeArray<ModuleElem<SyntaxToken>>()
            let mutable keepGoing = true

            while keepGoing do
                // Parse as many valid module elements as possible
                match many refModuleElem.Parser reader with
                | Ok elems ->
                    for e in elems do
                        result.Add(e)
                | Error _ -> ()

                // Check for remaining tokens
                match peekNextSyntaxToken reader with
                | Error _ -> keepGoing <- false
                | Ok tok ->
                    if tok.Token = Token.EOF then
                        keepGoing <- false
                    else
                        // Skip unparsed tokens until reaching the next module element boundary or EOF
                        let skipped = ResizeArray<SyntaxToken>()
                        let mutable skipping = true

                        while skipping do
                            match peekNextSyntaxToken reader with
                            | Error _ -> skipping <- false
                            | Ok tok ->
                                if StoppingTokens.afterModuleElem tok then
                                    skipping <- false
                                else
                                    match consumePeeked tok reader with
                                    | Ok t -> skipped.Add(t)
                                    | Error _ -> skipping <- false

                        if skipped.Count > 0 then
                            let startTok = skipped.[0]

                            let parseErr: ParseError<PositionedToken, ParseState> =
                                {
                                    Position = reader.Position
                                    Errors = Message "Unexpected token(s) in module"
                                }

                            reader.State <-
                                ParseState.addDiagnosticWithError
                                    DiagnosticCode.MissingModuleElem
                                    startTok
                                    parseErr
                                    reader.State

                            result.Add(ModuleElem.SkipsTokens(ImmutableArray.CreateRange(skipped)))
                        else
                            // Prevent infinite loop if no tokens can be parsed or skipped
                            keepGoing <- false

            Ok(ImmutableArray.CreateRange(result))

    let parse =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWOpen, ImportDecl.parse |>> ModuleElem.Import
                Token.KWHash, CompilerDirectiveDecl.parse |>> ModuleElem.CompilerDirective
            ]
            // Fallback for constructs that may begin with attributes (types, modules, bindings, etc.)
            (choiceL
                [
                    // Type definitions (including mutually recursive types linked by 'and')
                    parser {
                        let! typeToken, first = TypeDefn.parse
                        let! rest = many TypeDefn.parseAndContinuation
                        let builder = ImmutableArray.CreateBuilder(rest.Length + 1)
                        builder.Add(first)
                        builder.AddRange(rest)
                        return ModuleElem.Type(typeToken, builder.ToImmutable())
                    }

                    ExceptionDefn.parse |>> ModuleElem.Exception

                    // Module abbreviations must precede nested module definitions (e.g., `module M = A.B.C`)
                    ModuleAbbrev.parse |>> ModuleElem.ModuleAbbrev

                    (ModuleDefn.parse parseElemsWithRecovery) |>> ModuleElem.Module

                    ModuleFunctionOrValueDefn.parse |>> ModuleElem.FunctionOrValue

                    // Top-level expressions (scripts and interactive sessions)
                    Expr.parse |>> ModuleElem.Expression
                ]
                "ModuleElem")

    do refModuleElem.Set parse


[<RequireQualifiedAccess>]
module NamespaceDeclGroup =
    let parse =
        parser {
            let! nsTok = pNamespace
            let! globalTok = opt pGlobal

            match globalTok with
            | ValueSome gTok ->
                // Global namespace: `namespace global`
                let! elems = ModuleElem.parseElemsWithRecovery
                return NamespaceDeclGroup.Global(nsTok, gTok, elems)

            | ValueNone ->
                // Named namespace: `namespace [rec] Path`
                let! isRec = opt pRec
                let! ident = recoverLongIdent "Expected namespace identifier" LongIdent.parse
                let! elems = ModuleElem.parseElemsWithRecovery
                return NamespaceDeclGroup.Named(nsTok, isRec, ident, elems)
        }
