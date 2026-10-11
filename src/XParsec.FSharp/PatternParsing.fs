namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken

module Pat =

    let private errNotPrefixPatOp: ErrorType<PositionedToken, ParseState> =
        Message "Not a prefix pattern operator"

    let private errNotValidRhsPatOp: ErrorType<PositionedToken, ParseState> =
        Message "Not a valid RHS pattern operator"

    let private errRecordPattern: ErrorType<PositionedToken, ParseState> =
        Message "Record pattern"

    let private errPositionalOnlyCtorPat: ErrorType<PositionedToken, ParseState> =
        Message "positional-only ctor pattern"

    [<RequireQualifiedAccess>]
    type PatAux =
        | Type of Type<SyntaxToken>
        | AsIdent of SyntaxToken

    // --- Operator Precedence & Pratt Helpers ---
    // Used by:
    // - `PatOperatorParser`: Standard patterns (including `:` type annotations).
    // - `PatHeadOperatorParser`: Head patterns for `let` bindings (excludes `:` so type annotations attach to the binding).

    let private tuplePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Comma)
    let private asPrecedence = BindingPower.fromLevel (int PrecedenceLevel.As)
    let private semiPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Semicolon)
    let private pipePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Pipe)
    let private andPrecedence = BindingPower.fromLevel (int PrecedenceLevel.LogicalAnd)
    let private colonPrecedence = BindingPower.fromLevel (int PrecedenceLevel.TypeTest)
    let private consPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Cons)
    let private parenPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Parens)

    let private structPrecedence =
        BindingPower.fromLevel (int PrecedenceLevel.HighApplication)

    let private completeInfix (l: Pat<SyntaxToken>) (op: SyntaxToken) (r: Pat<SyntaxToken>) =
        match op.Token with
        | Token.OpBar -> Pat.Or(l, op, r)
        | Token.OpAmp -> Pat.And(l, op, r)
        | Token.KWColonColon -> Pat.Cons(l, op, r)
        | _ -> failwithf "Unexpected infix pattern operator: %A" op

    let private completeTuple (elements: ResizeArray<Pat<SyntaxToken>>) (ops: ResizeArray<SyntaxToken>) =
        Pat.Tuple(ImmutableArray.CreateRange(elements), ImmutableArray.CreateRange(ops))

    let private completeTyped (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
        match aux with
        | PatAux.Type t -> Pat.Typed(l, op, t)
        | _ -> failwith "Expected Type aux for Typed pattern"

    let private completeAs (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
        match aux with
        | PatAux.AsIdent ident -> Pat.As(l, op, ident)
        | _ -> failwith "Expected Ident aux for As pattern"

    let private completeParen (l: SyntaxToken) (p: Pat<SyntaxToken>) (r: SyntaxToken) =
        Pat.EnclosedBlock(ParenKind.Paren l, p, r)

    let private completeStruct (op: SyntaxToken) (r: Pat<SyntaxToken>) =
        match r with
        | Pat.EnclosedBlock(ParenKind.Paren l, Pat.Tuple(elements, ops), r) -> Pat.StructTuple(op, l, elements, ops, r)
        | Pat.EnclosedBlock(ParenKind.Paren l, inner, r) ->
            // `struct (x)`: parsed as a 1-element struct tuple (rejected later during type checking).
            Pat.StructTuple(op, l, ImmutableArray.Create(inner), ImmutableArray.Empty, r)
        | _ ->
            // `struct x`: malformed without parens, but parsed with synthetic parens for error recovery.
            let lParen = virtualToken (PositionedToken.Create(Token.KWLParen, op.StartIndex))
            let rParen = virtualToken (PositionedToken.Create(Token.KWRParen, op.StartIndex))
            Pat.StructTuple(op, lParen, ImmutableArray.Create(r), ImmutableArray.Empty, rParen)

    let private completeElems (exprs: ResizeArray<Pat<_>>) ops =
        Pat.Elems(ImmutableArray.CreateRange(exprs), ImmutableArray.CreateRange(ops))

    /// Emits a virtual ';' for newline-separated pattern elements in lists or arrays
    /// when the next token is at the current offside indentation.
    let private pSepVirtPat: FSParser<SyntaxToken> =
        let failSep =
            fail (Message "Expected ';' or newline at the same indent for pattern sequencing")

        parser {
            match! peekNextSyntaxToken with
            | t when t.Token = Token.EOF -> return! failSep
            | t when t.Token = Token.OpSemicolon -> return! failSep
            | t ->
                if TokenInfo.canStartPattern t.Token then
                    let! indent = currentIndent
                    let! state = getUserState

                    let atContextIndent =
                        match state.Context with
                        | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                        | [] -> indent = 0

                    if atContextIndent then
                        return virtualToken (PositionedToken.Create(Token.OpSemicolon, t.StartIndex))
                    else
                        return! failSep
                else
                    return! failSep
        }

    /// Separator parser for tuple pattern elements (',').
    let private pPatTupleComma: FSParser<SyntaxToken> =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpComma) "','"

    /// Separator parser for sequence pattern elements (';' or a virtual newline separator).
    let private pPatSemicolon: FSParser<SyntaxToken> =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpSemicolon) "';'"
        <|> pSepVirtPat

    let private pTypeRhs = Type.parse |>> PatAux.Type

    let private pAsRhs =
        parser {
            // 'as' was already consumed by the operator parser; consume the binding identifier.
            let! ident = nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "identifier after 'as'"
            return PatAux.AsIdent ident
        }

    let private patLhsParser =
        parser {
            let! token = nextSyntaxToken

            match token.Token with
            | Token.KWLParen ->
                // Prefix operator for parenthesized patterns: ( ... )
                let p = preturn token
                let rParen = virtualToken (PositionedToken.Create(Token.KWRParen, 0))
                let op = Enclosed(token, p, parenPrecedence, rParen, pRParen, completeParen)
                return op
            | Token.KWStruct ->
                // Prefix operator for struct patterns: struct (...)
                let p = preturn token
                let op = Prefix(token, p, structPrecedence, completeStruct)
                return op
            | _ -> return! fail errNotPrefixPatOp
        }

    // RHS operator factories. Kept as top-level functions to avoid closure allocations.
    let private mkOpBar (tok: SyntaxToken) =
        InfixLeft(tok, preturn tok, pipePrecedence, completeInfix)

    let private mkOpAmp (tok: SyntaxToken) =
        InfixLeft(tok, preturn tok, andPrecedence, completeInfix)

    let private mkColonColon (tok: SyntaxToken) =
        InfixRight(tok, preturn tok, consPrecedence, completeInfix)

    let private mkOpColon (tok: SyntaxToken) =
        InfixMapped(tok, preturn tok, colonPrecedence, pTypeRhs, completeTyped)

    let private mkKWAs (tok: SyntaxToken) =
        InfixMapped(tok, preturn tok, asPrecedence, pAsRhs, completeAs)

    let private mkOpComma (tok: SyntaxToken) =
        InfixNary(tok, pPatTupleComma, tuplePrecedence, false, completeTuple)

    let private mkSemiCompletes (tok: SyntaxToken) =
        InfixNary(tok, pPatSemicolon, semiPrecedence, true, completeElems)

    let private mkSemiBare (tok: SyntaxToken) =
        InfixNary(tok, pPatSemicolon, semiPrecedence, false, completeElems)

    // Static route tables for RHS pattern operators.
    let private patRhsRoutesFull
        : struct (Token *
          (SyntaxToken -> RHSOperator<SyntaxToken, PatAux, Pat<SyntaxToken>, PositionedToken, ParseState, FSReadable>))[] =
        [|
            struct (Token.OpComma, mkOpComma)
            struct (Token.OpBar, mkOpBar)
            struct (Token.OpAmp, mkOpAmp)
            struct (Token.OpSemicolon, mkSemiCompletes)
            struct (Token.KWColonColon, mkColonColon)
            struct (Token.OpColon, mkOpColon)
            struct (Token.KWAs, mkKWAs)
        |]

    let private patRhsRoutesHead
        : struct (Token *
          (SyntaxToken -> RHSOperator<SyntaxToken, PatAux, Pat<SyntaxToken>, PositionedToken, ParseState, FSReadable>))[] =
        [|
            struct (Token.OpComma, mkOpComma)
            struct (Token.OpBar, mkOpBar)
            struct (Token.OpAmp, mkOpAmp)
            struct (Token.OpSemicolon, mkSemiBare)
            struct (Token.KWColonColon, mkColonColon)
            struct (Token.KWAs, mkKWAs)
        |]

    // Dispatches RHS operators by looking up the next token in the route table.
    let private patRhsParserSpecialist
        (routes:
            struct (Token *
            (SyntaxToken -> RHSOperator<SyntaxToken, PatAux, Pat<SyntaxToken>, PositionedToken, ParseState, FSReadable>))[])
        (semiCompletesElems: bool)
        : FSParser<_> =
        fun reader ->
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok t ->
                let token = t.Token
                let mutable handler = ValueNone
                let mutable i = 0

                while handler.IsNone && i < routes.Length do
                    let struct (k, h) = routes.[i]

                    if k = token then
                        handler <- ValueSome h

                    i <- i + 1

                match handler with
                | ValueSome h ->
                    match consumePeeked t reader with
                    | Error e -> Error e
                    | Ok tok -> Ok(h tok)
                | ValueNone ->
                    if token = Token.EOF then
                        fail errNotValidRhsPatOp reader
                    elif TokenInfo.canStartPattern token then
                        // Insert virtual ';' if the next pattern starts at the current indentation context.
                        let state = reader.State
                        let idx = int reader.Index * 1<token>
                        let indent = ParseState.getIndent state idx

                        let atContextIndent =
                            match state.Context with
                            | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                            | [] -> indent = 0

                        if atContextIndent then
                            let virt = virtualToken (PositionedToken.Create(Token.OpSemicolon, t.StartIndex))

                            Ok(InfixNary(virt, pPatSemicolon, semiPrecedence, semiCompletesElems, completeElems))
                        else
                            fail errNotValidRhsPatOp reader
                    else
                        fail errNotValidRhsPatOp reader

    /// Operator parser for standard patterns (e.g. `(x : int)`).
    /// Includes `:` for inline type annotations.
    type PatOperatorParser() =
        static let rhsParser = patRhsParserSpecialist patRhsRoutesFull true

        interface Operators<SyntaxToken, PatAux, Pat<SyntaxToken>, PositionedToken, ParseState, FSReadable> with
            member _.LhsParser = patLhsParser
            member _.RhsParser = rhsParser

    /// Operator parser for `let` binding head patterns (e.g. `let x : int = 5`).
    /// Excludes `:` so type annotations belong to the binding return type instead of the pattern.
    type PatHeadOperatorParser() =
        static let rhsParser = patRhsParserSpecialist patRhsRoutesHead false

        interface Operators<SyntaxToken, PatAux, Pat<SyntaxToken>, PositionedToken, ParseState, FSReadable> with
            member _.LhsParser = patLhsParser
            member _.RhsParser = rhsParser

    let private refPat = FSRefParser<Pat<SyntaxToken>>()
    let private refPatSeqBlock = FSRefParser<Pat<SyntaxToken>>()
    let private refFieldPat = FSRefParser<Pat<SyntaxToken>>()
    let private refUnionFieldPat = FSRefParser<Pat<SyntaxToken>>()
    let private refPatAtomic = FSRefParser<Pat<SyntaxToken>>()
    let private refPatAtomicBindingArg = FSRefParser<Pat<SyntaxToken>>()

    let private pEnclosed =
        let completeEmpty l r = Pat.EmptyBlock(l, r)
        let completeEnclosed l e r = Pat.EnclosedBlock(l, e, r)
        let skipsTokens toks = Pat.SkipsTokens(toks)
        pEnclosed completeEmpty completeEnclosed Pat.Missing skipsTokens

    let pParenPat =
        pEnclosed
            pLParen
            Token.KWRParen
            ParenKind.Paren
            OffsideContext.Paren
            DiagnosticCode.ExpectedRParen
            refPat.Parser

    let pListPat =
        pEnclosed
            pLBracket
            Token.KWRBracket
            ParenKind.List
            OffsideContext.Bracket
            DiagnosticCode.ExpectedRBracket
            refPatSeqBlock.Parser

    let pArrayPat =
        pEnclosed
            pLArrayBracket
            Token.KWRArrayBracket
            ParenKind.Array
            OffsideContext.BracketBar
            DiagnosticCode.ExpectedRArrayBracket
            refPatSeqBlock.Parser

    // Handles trailing '|' and 'as' clauses for record and union fields.
    // (These operators have lower precedence than field delimiters, so they are handled separately).
    let private pBarToken =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpBar) "'|'"

    let private pOrAsChain (altParser: FSParser<Pat<SyntaxToken>>) (basePat: Pat<SyntaxToken>) =
        parser {
            let! orAlts =
                many (
                    parser {
                        let! barTok = pBarToken
                        // Isolate each '|' alternative in a SeqBlock so argument parsing doesn't spill into lower-indented fields.
                        let! altPat = withContext OffsideContext.SeqBlock altParser
                        return struct (barTok, altPat)
                    }
                )

            let p =
                if orAlts.IsEmpty then
                    basePat
                else
                    orAlts
                    |> Seq.fold (fun acc struct (barTok, altPat) -> Pat.Or(acc, barTok, altPat)) basePat

            let! asClause =
                opt (
                    parser {
                        let! asTok = pAs

                        let! ident =
                            nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "identifier after 'as'"

                        return struct (asTok, ident)
                    }
                )

            return
                match asClause with
                | ValueSome(asTok, ident) -> Pat.As(p, asTok, ident)
                | ValueNone -> p
        }

    let pFieldPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! basePat = withContext OffsideContext.SeqBlock refFieldPat.Parser
            let! p = pOrAsChain refFieldPat.Parser basePat
            return FieldPat(lid, eq, p)
        }

    let pRecordPat: FSParser<Pat<SyntaxToken>> =
        fun reader ->
            match pLBrace reader with
            | Error e -> Error e
            | Ok l ->
                let savedState = reader.State

                // Brace context disables offside checks so closing '}' is not blocked by indentation rules.
                let braceEntry: Offside =
                    {
                        Context = OffsideContext.Brace
                        Indent = 0
                        Token = l.PositionedToken
                    }

                reader.State <- ParseState.pushOffside braceEntry reader.State

                let innerParser =
                    parser {
                        let! fields, seps = withContext OffsideContext.SeqBlock (sepEndBy1 pFieldPat pRecordFieldSep)
                        let! r = pRBrace
                        return Pat.Record(l, fields, seps, r)
                    }

                match innerParser reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside braceEntry reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail errRecordPattern reader

    /// Parses a single named field in a union case pattern (e.g. `FieldName = pat`).
    let private pUnionNamedArgPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! basePat = withContext OffsideContext.SeqBlock refUnionFieldPat.Parser
            let! p = pOrAsChain refUnionFieldPat.Parser basePat
            return UnionArgPat.Named(lid, eq, p)
        }

    /// Parses a single positional argument in a union case pattern.
    let private pUnionPositionalArgPat =
        parser {
            let! basePat = withContext OffsideContext.SeqBlock refUnionFieldPat.Parser
            let! p = pOrAsChain refUnionFieldPat.Parser basePat
            return UnionArgPat.Positional p
        }

    /// Parses a union case argument (either named or positional).
    let private pUnionArgPat = pUnionNamedArgPat <|> pUnionPositionalArgPat

    /// Parses constructor arguments with named fields: `(field1 = pat1, field2 = pat2)`.
    /// Backtracks if no arguments are named fields so the caller can fall back to positional arguments.
    let private tryNamedFieldPatsAfterLid
        (lid: LongIdent<SyntaxToken>)
        (postLidPos: Position<ParseState>)
        : FSParser<Pat<SyntaxToken>> =
        fun reader ->
            match pLParen reader with
            | Error e -> Error e
            | Ok lParen ->
                // Paren context disables offside checks so inner arguments can span multiple lines.
                let parenEntry: Offside =
                    {
                        Context = OffsideContext.Paren
                        Indent = 0
                        Token = lParen.PositionedToken
                    }

                let savedState = reader.State
                reader.State <- ParseState.pushOffside parenEntry reader.State

                let innerParser =
                    parser {
                        let! args, seps =
                            withContext OffsideContext.SeqBlock (sepBy1 pUnionArgPat (pComma <|> pRecordFieldSep))

                        let! rParen = pRParen

                        let hasNamed =
                            args
                            |> Seq.exists (
                                function
                                | UnionArgPat.Named _ -> true
                                | _ -> false
                            )

                        if hasNamed then
                            return Pat.NamedFieldPats(lid, lParen, args, seps, rParen)
                        else
                            return! fail errPositionalOnlyCtorPat
                    }

                match innerParser reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside parenEntry reader.State
                    Ok result
                | Error _ as e ->
                    // Restore state and position so the positional fallback can re-parse the '('.
                    reader.State <- savedState
                    reader.Position <- postLidPos
                    e

    /// Parses identifier patterns, resolving between named-field constructor patterns
    /// and standard positional patterns without re-parsing the leading identifier.
    let pNamed: FSParser<Pat<SyntaxToken>> =
        fun reader ->
            match LongIdent.parse reader with
            | Error e -> Error e
            | Ok lid ->
                let postLidPos = reader.Position

                // Try named-field pattern (e.g. `MyType(Field = pat)`); fall back to positional args.
                match tryNamedFieldPatsAfterLid lid postLidPos reader with
                | Ok result -> Ok result
                | Error _ ->
                    // Positional path: parses trailing curried arguments for active patterns or constructors.
                    match many refPatAtomicBindingArg.Parser reader with
                    | Error e -> Error e
                    | Ok args ->
                        match lid.Idents.Length, args.IsEmpty with
                        | 1, true -> Ok(Pat.NamedSimple(lid.Idents.[0]))
                        | _ -> Ok(Pat.Named(lid, args))

    /// Parses parenthesized operator patterns with trailing arguments (e.g. `(|Op|) arg1 arg2` or `((+) x)`).
    let private pParenOpHeadPat =
        parser {
            let! l = pLParen
            let! op = OpName.parse
            let! r = pRParen
            let ident = IdentOrOp.ParenOp(l, op, r)
            let! args = many refPatAtomicBindingArg.Parser

            if args.IsEmpty then
                return Pat.Op ident
            else
                return Pat.OpNamed(ident, args)
        }

    let private pParenOrOpHeadPat = pParenOpHeadPat <|> pParenPat

    let pTypeTestPat =
        parser {
            let! op = pTypeTest
            let! t = Type.parseAtomic
            // Parse optional ':? Type as pat' clause.
            let! asClause =
                opt (
                    parser {
                        let! asTok = pAs
                        let! pat = pNamed
                        return struct (asTok, pat)
                    }
                )

            match asClause with
            | ValueSome(asTok, pat) -> return Pat.TypeTestAs(op, t, asTok, pat)
            | ValueNone -> return Pat.TypeTest(op, t)
        }

    let pAttributesPat =
        parser {
            let! attrs = Attributes.parse
            let! pat = refPat.Parser
            return Pat.Attributed(attrs, pat)
        }

    let pConstPat =
        parser {
            let! c = Constant.parse
            return Pat.Const c
        }

    let private isStringClose (tok: Token) =
        match tok with
        | Token.StringClose
        | Token.ByteArrayClose
        | Token.VerbatimStringClose
        | Token.VerbatimByteArrayClose
        | Token.String3Close
        | Token.UnterminatedStringLiteral
        | Token.UnterminatedVerbatimStringLiteral
        | Token.UnterminatedString3Literal -> true
        | _ -> false

    let private isStringTextFragment (tok: Token) =
        match tok with
        | Token.StringFragment
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote -> true
        | _ -> false

    let private stringKindOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringOpen -> StringKind.String t
        | Token.VerbatimStringOpen -> StringKind.VerbatimString t
        | Token.String3Open -> StringKind.String3 t
        | _ -> invalidOp $"Not a string open token: {t.Token}"

    let pStringPat =
        let rec loop (parts: ResizeArray<StringPart<SyntaxToken>>) reader =
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok t when isStringTextFragment t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fragment ->
                    parts.Add(StringPart.Text fragment)
                    loop parts reader
            | Ok t when t.Token = Token.EscapeSequence ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok esc ->
                    parts.Add(StringPart.EscapeSequence esc)
                    loop parts reader
            | Ok t when t.Token = Token.FormatPlaceholder ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fmt ->
                    parts.Add(StringPart.FormatSpecifier fmt)
                    loop parts reader
            | Ok t when
                t.Token = Token.InvalidFormatPlaceholder
                || t.Token = Token.InvalidFormatPercents
                ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok _ -> loop parts reader
            | Ok _ -> Ok parts

        parser {
            let! opening =
                nextSyntaxTokenSatisfiesLMsg
                    (fun t ->
                        match t.Token with
                        | Token.StringOpen
                        | Token.VerbatimStringOpen
                        | Token.String3Open -> true
                        | _ -> false
                    )
                    "Expected string literal open"

            let kind = stringKindOfToken opening
            let! parts = loop (ResizeArray())

            let! closing = nextSyntaxTokenSatisfiesLMsg (fun t -> isStringClose t.Token) "Expected string close"

            return Pat.String(kind, ImmutableArray.CreateRange(parts), closing)
        }

    let pWildcardPat = pWildcard |>> Pat.Wildcard
    let pNullPat = pNull |>> Pat.Null

    let private pStructPat =
        parser {
            let! structTok = pStruct
            let! pat = pParenPat

            match pat with
            | Pat.EnclosedBlock(ParenKind.Paren l, Pat.Tuple(elements, ops), r) ->
                return Pat.StructTuple(structTok, l, elements, ops, r)
            | Pat.EnclosedBlock(ParenKind.Paren l, inner, r) ->
                return Pat.StructTuple(structTok, l, ImmutableArray.Create(inner), ImmutableArray.Empty, r)
            | _ ->
                // Synthesize virtual parentheses for error recovery if pParenPat produced a missing/skipped block.
                let lParen =
                    virtualToken (PositionedToken.Create(Token.KWLParen, structTok.StartIndex))

                let rParen =
                    virtualToken (PositionedToken.Create(Token.KWRParen, structTok.StartIndex))

                return Pat.StructTuple(structTok, lParen, ImmutableArray.Create(pat), ImmutableArray.Empty, rParen)
        }

    let private pOptionalPat =
        parser {
            let! qmark = pQuestionMark
            let! pat = pNamed
            return Pat.Optional(qmark, pat)
        }

    // Parses quotation patterns (e.g. `<@ expr @>`), wrapping the inner expression in Pat.Expr.
    let private pInnerQuotedExpr = refExprSeqBlock.Parser |>> Pat.Expr

    let private pQuoteTypedPat =
        pEnclosed
            pQuotationTypedLeft
            Token.OpQuotationTypedRight
            ParenKind.Quoted
            OffsideContext.Quote
            DiagnosticCode.ExpectedQuotationTypedRight
            pInnerQuotedExpr

    let private pQuoteUntypedPat =
        pEnclosed
            pQuotationUntypedLeft
            Token.OpQuotationUntypedRight
            ParenKind.DoubleQuoted
            OffsideContext.Quote
            DiagnosticCode.ExpectedQuotationUntypedRight
            pInnerQuotedExpr

    /// Parses an identifier pattern without consuming trailing arguments (for parameter lists).
    let private pNamedNoArgs =
        parser {
            let! lid = LongIdent.parse

            match lid.Idents.Length with
            | 1 -> return Pat.NamedSimple(lid.Idents.[0])
            | _ -> return Pat.Named(lid, ImmutableArray.Empty)
        }

    let parseAtomic =
        dispatchNextSyntaxTokenFallback
            [
                Token.Identifier, pNamed
                Token.Wildcard, pWildcardPat
                Token.KWLParen, pParenOrOpHeadPat
                Token.KWLBracket, pListPat
                Token.KWLAttrBracket, pAttributesPat
                Token.KWLBrace, pRecordPat
                Token.KWLArrayBracket, pArrayPat
                Token.KWNull, pNullPat
                Token.KWStruct, pStructPat
                Token.StringOpen, pStringPat
                Token.VerbatimStringOpen, pStringPat
                Token.String3Open, pStringPat
                Token.BacktickedIdentifier, pNamed
                Token.OpTypeTest, pTypeTestPat
                Token.OpDynamic, pOptionalPat
                Token.UnterminatedBacktickedIdentifier, pNamed
            ]
            pConstPat

    do refPatAtomic.Set parseAtomic

    let parse = Operator.parser parseAtomic (PatOperatorParser())

    let parseSeqBlock = withContext OffsideContext.SeqBlock parse

    /// Parses head patterns for `let` bindings (excludes `:` so return type annotations are not captured).
    let parseHead = Operator.parser parseAtomic (PatHeadOperatorParser())

    // Record field values: allows standard pattern operators except ';' (which separates fields).
    let private parseFieldPat =
        Operator.parserAt (BindingPower.fromLevel (int PrecedenceLevel.Semicolon + 1)) parseAtomic (PatOperatorParser())

    // Union field values: allows standard pattern operators except ',' and ';'.
    let private parseUnionFieldPat =
        Operator.parserAt (BindingPower.fromLevel (int PrecedenceLevel.Comma + 1)) parseAtomic (PatOperatorParser())

    /// Atomic pattern parser for parameter lists (identifiers do not consume trailing arguments).
    let parseAtomicBindingArg =
        dispatchNextSyntaxTokenFallback
            [
                Token.Identifier, pNamedNoArgs
                Token.Wildcard, pWildcardPat
                Token.KWLParen, pParenOrOpHeadPat
                Token.KWLBracket, pListPat
                Token.KWLAttrBracket, pAttributesPat
                Token.KWLBrace, pRecordPat
                Token.KWLArrayBracket, pArrayPat
                Token.KWNull, pNullPat
                Token.KWStruct, pStructPat
                Token.StringOpen, pStringPat
                Token.VerbatimStringOpen, pStringPat
                Token.String3Open, pStringPat
                Token.BacktickedIdentifier, pNamedNoArgs
                Token.OpTypeTest, pTypeTestPat
                Token.OpDynamic, pOptionalPat
                Token.OpQuotationTypedLeft, pQuoteTypedPat
                Token.OpQuotationUntypedLeft, pQuoteUntypedPat
                Token.UnterminatedBacktickedIdentifier, pNamedNoArgs
            ]
            pConstPat

    let parseAtomicBindingArgMany1 = many1 parseAtomicBindingArg

    do refPat.Set parse
    do refPatSeqBlock.Set parseSeqBlock
    do refFieldPat.Set parseFieldPat
    do refUnionFieldPat.Set parseUnionFieldPat
    do refPatAtomicBindingArg.Set parseAtomicBindingArg


[<RequireQualifiedAccess>]
module PatternGuard =
    let parse: FSParser<PatternGuard<SyntaxToken>> =
        parser {
            let! w = pWhen
            // Bounded at `->` precedence so `->` is not consumed as part of the guard expression.
            let! e =
                refExprGuard.Parser
                |> recoverWith
                    StoppingTokens.afterPattern
                    DiagnosticCode.MissingExpression
                    (missingOrSkipped Expr.Missing Expr.SkipsTokens)

            return PatternGuard(w, e)
        }

[<RequireQualifiedAccess>]
module Rule =
    /// Parses a single match rule: `pattern [when guard] -> expression`
    let parse: FSParser<Rule<SyntaxToken>> =
        parser {
            let! pat =
                Pat.parse
                |> recoverWith
                    StoppingTokens.afterPattern
                    DiagnosticCode.MissingPattern
                    (missingOrSkipped Pat.Missing Pat.SkipsTokens)

            let! guard = opt PatternGuard.parse
            let! arrow = pArrowRight

            let! expr =
                refTypedSeqExprBlock.Parser
                |> recoverWith
                    StoppingTokens.afterRule
                    DiagnosticCode.MissingExpression
                    (missingOrSkipped Expr.Missing Expr.SkipsTokens)

            return Rule.Rule(pat, guard, arrow, expr)
        }

[<RequireQualifiedAccess>]
module Rules =
    let private pRule =
        recoverWith
            StoppingTokens.afterRule
            DiagnosticCode.MissingRule
            (missingOrSkipped Rule.Missing Rule.SkipsTokens)
            Rule.parse

    let parse: FSParser<Rules<SyntaxToken>> =
        parser {
            let! firstBar = opt pBar
            let! rules, bars = sepBy1 pRule pBar
            return Rules(firstBar, rules, bars)
        }
