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
open XParsec.FSharp.Parser.ParseState

module ElifBranches =
    [<Struct; RequireQualifiedAccess>]
    type private ElIfTok =
        | Elif of el: SyntaxToken
        | Else of el: SyntaxToken
        | ElseIf of el: SyntaxToken * ifTok: SyntaxToken

    let private pElifOrElseIf =
        dispatchNextSyntaxTokenL
            [
                Token.KWElif,
                parser {
                    let! elifTok = pElif
                    return ElIfTok.Elif elifTok
                }
                Token.KWElse,
                parser {
                    let! elseTok = pElse

                    match! peekNextSyntaxToken with
                    | t when t.Token = Token.KWIf ->
                        // Collapse `else if` into a single chain arm if it's on the same line
                        // or if `if` aligns with or undents to `else`. If `if` is indented further
                        // to the right, treat it as a nested `if` inside the else body.
                        let! state = getUserState

                        let collapse =
                            match elseTok.Index, t.Index with
                            | TokenIndex.Regular elseIdx, TokenIndex.Regular ifIdx ->
                                let elseCol = ParseState.getIndent state elseIdx
                                let ifCol = ParseState.getIndent state ifIdx
                                let elseLine = ParseState.findLineNumber state elseIdx
                                let ifLine = ParseState.findLineNumber state ifIdx
                                elseLine = ifLine || ifCol <= elseCol
                            | _ -> true

                        if collapse then
                            let! ifTok = consumePeeked t
                            return ElIfTok.ElseIf(elseTok, ifTok)
                        else
                            return ElIfTok.Else elseTok
                    | _ -> return ElIfTok.Else elseTok
                }
            ]
            "Expected 'elif' or 'else'"

    // Anchors the branch's 'If' and 'Then' contexts at (indent + 1) to allow
    // multi-line conditions that undent on following lines to remain within scope.
    let pConditionThen (indent: int) (armKeyword: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        let pCond =
            withContextAt OffsideContext.If (indent + 1) armKeyword.PositionedToken refExpr.Parser
        // Grammar: THEN typedSeqExprBlock
        let pThenExpr =
            withContextAt OffsideContext.Then (indent + 1) armKeyword.PositionedToken refTypedSeqExprBlock.Parser

        match pCond reader with
        | Ok condition ->
            match pThen reader with
            | Ok thenTok ->
                match pThenExpr reader with
                | Ok expr -> Ok(condition, thenTok, expr)
                | Error e -> Error e
            | Error e -> Error e
        | Error e -> Error e

    // Grammar: ELSE typedSeqExprBlock
    let private pElseExpr = withContext OffsideContext.Else refTypedSeqExprBlock.Parser

    let private getIndent (reader: Reader<PositionedToken, ParseState, _>) (tok: SyntaxToken) =
        match tok.Index with
        | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
        | TokenIndex.Virtual -> 0

    let rec private parseBranches (acc: ResizeArray<_>) (reader: Reader<PositionedToken, ParseState, _>) =
        match pElifOrElseIf reader with
        | Ok(ElIfTok.Elif elifTok) ->
            let indent = getIndent reader elifTok

            match pConditionThen indent elifTok reader with
            | Ok(condition, thenTok, expr) ->
                acc.Add(ElifBranch.Elif(elifTok, condition, thenTok, expr))
                parseBranches acc reader
            | Error e -> Error e

        | Ok(ElIfTok.ElseIf(elseTok, ifTok)) ->
            // Use the leftmost keyword column as the alignment indent
            let indent = min (getIndent reader elseTok) (getIndent reader ifTok)

            match pConditionThen indent elseTok reader with
            | Ok(condition, thenTok, expr) ->
                acc.Add(ElifBranch.ElseIf(elseTok, ifTok, condition, thenTok, expr))
                parseBranches acc reader
            | Error e -> Error e

        | Ok(ElIfTok.Else elseTok) ->
            match pElseExpr reader with
            | Ok expr -> Ok struct (ImmutableArray.CreateRange acc, ValueSome(ElseBranch.ElseBranch(elseTok, expr)))
            | Error e -> Error e

        | Error e -> Ok struct (ImmutableArray.CreateRange acc, ValueNone)

    let parse: FSParser<_> = fun reader -> parseBranches (ResizeArray()) reader

module Binding =
    let private errNoTrailingTypeAnnotation: ErrorType<PositionedToken, ParseState> =
        Message "no trailing type annotation"

    let private errNotNamedFieldCtorPatStart: ErrorType<PositionedToken, ParseState> =
        Message "not the start of a named-field ctor pattern"

    let private pMutableTok =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWMutable) "Expected 'mutable'"

    /// Creates a pattern from an identifier or operator token.
    let private patternOfIdentOrOp (identOrOp: IdentOrOp<SyntaxToken>) : Pat<SyntaxToken> =
        match identOrOp with
        | IdentOrOp.Ident t -> Pat.NamedSimple t
        | _ -> Pat.Op identOrOp

    // Parses a static-optimization constraint: `^T : Type` or `^T : struct`.
    let private pStaticOptimizationConstraint =
        parser {
            let! typar = Typar.parse

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWStruct ->
                // Bare `'T struct` without a colon
                let! structTok = consumePeeked t
                return StaticOptimizationConstraint.WhenTyparIsStruct(typar, ValueNone, structTok)
            | _ ->
                let! colon = pColon

                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWStruct ->
                    let! structTok = consumePeeked t
                    return StaticOptimizationConstraint.WhenTyparIsStruct(typar, ValueSome colon, structTok)
                | _ ->
                    let! rhsType = Type.parse
                    return StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar, colon, rhsType)
        }

    let private pAndConstraint =
        parser {
            let! andTok = pAnd
            let! c = pStaticOptimizationConstraint
            return struct (andTok, c)
        }

    // Parses `when c1 and c2 ... = optimizedExpr`
    let private pStaticOptimizationClause =
        parser {
            let! whenTok = pWhen
            let! firstC = pStaticOptimizationConstraint
            let! restPairs = many pAndConstraint
            let! equalsTok = pEquals
            let! optExpr = refExprSeqBlock.Parser

            return
                {
                    WhenToken = whenTok
                    Constraint = firstC
                    AndConstraints = restPairs
                    EqualsToken = equalsTok
                    OptimizedExpr = optExpr
                }
        }

    // Collects any trailing `when` static-optimization clauses onto the binding body.
    let private pChainStaticOptimizations (baseExpr: Expr<SyntaxToken>) =
        parser {
            let! clauses = many pStaticOptimizationClause

            match clauses.Length with
            | 0 -> return baseExpr
            | _ -> return Expr.LibraryOnlyStaticOptimization(baseExpr, clauses)
        }

    let private pBindingBody =
        // Captures the colon column so the return type can be parsed in a stricter
        // offside context (colonCol + 1), preventing multi-line suffix parsing issues.
        let pColonPeek (reader: FSReader) =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.OpColon ->
                let col = ParseState.getIndent reader.State (reader.Index * 1<token>)

                match consumePeeked t reader with
                | Ok colon -> Ok(struct (colon, col))
                | Error e -> Error e
            | Ok _ -> fail errNoTrailingTypeAnnotation reader
            | Error e -> Error e

        parser {
            let! expr = refExprSeqBlock.Parser
            let! maybeColon = opt pColonPeek

            let! annotated =
                parser {
                    match maybeColon with
                    | ValueSome(colon, colonCol) ->
                        let! typ = withContextAt OffsideContext.Let (colonCol + 1) colon.PositionedToken Type.parse

                        return Expr.TypeAnnotation(expr, colon, typ)
                    | ValueNone -> return expr
                }

            return! pChainStaticOptimizations annotated
        }

    // Lookahead guard to identify named-field pattern destructuring `(Identifier = ...)`,
    // preventing `parseFunction` from incorrectly matching value-binding patterns.
    let private pNamedFieldCtorPatStart =
        lookAhead (
            parser {
                let! t1 = nextSyntaxToken
                let! t2 = nextSyntaxToken
                let! t3 = nextSyntaxToken

                if
                    t1.Token = Token.KWLParen
                    && (t2.Token = Token.Identifier || t2.Token = Token.BacktickedIdentifier)
                    && t3.Token = Token.OpEquality
                then
                    return ()
                else
                    return! fail errNotNamedFieldCtorPatStart
            }
        )

    /// Parse a function-style binding: [inline] [access] identOrOp [typar-defns] pat+ [: returnType] = expr
    let parseFunction attrs =
        let pBody = pBindingBody

        parser {
            let! inlineTok = opt pInline
            let! access = opt Access.parse
            let! identOrOp = IdentOrOp.parse
            do! notFollowedByL pNamedFieldCtorPatStart "named-field destructure"
            let! typarDefns = opt TyparDefns.parse
            // Operators and generic values allow zero argument patterns;
            // standard named function definitions require at least one.
            let! argumentPats =
                match identOrOp with
                | IdentOrOp.ParenOp _ -> many Pat.parseAtomicBindingArg
                | IdentOrOp.Ident _ when typarDefns.IsSome -> many Pat.parseAtomicBindingArg
                | IdentOrOp.Ident _ -> Pat.parseAtomicBindingArgMany1

            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = pBody

            return
                {
                    attributes = attrs
                    inlineToken = inlineTok
                    mutableToken = ValueNone
                    access = access
                    pattern = patternOfIdentOrOp identOrOp
                    typarDefns = typarDefns
                    argumentPats = argumentPats
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse a value-style binding: `[mutable] [access] pat [typar-defns] [: returnType] = expr`, or
    /// `inline [access] pat [: returnType] = expr`. An inline binding with explicit type parameters is
    /// a function-style binding.
    let parseValue attrs =
        let pBody = pBindingBody

        parser {
            let! inlineTok = opt pInline

            let! mut =
                match inlineTok with
                | ValueSome _ -> preturn ValueNone
                | ValueNone -> opt pMutableTok

            let! access = opt Access.parse
            let! pat = Pat.parseHead

            let! typarDefns =
                match inlineTok with
                | ValueSome _ -> preturn ValueNone
                | ValueNone -> opt TyparDefns.parse

            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = pBody

            return
                {
                    attributes = attrs
                    inlineToken = inlineTok
                    mutableToken = mut
                    access = access
                    pattern = pat
                    typarDefns = typarDefns
                    argumentPats = ImmutableArray.Empty
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse either a function or a value binding
    let parse attrs : FSParser<Binding<_>> =
        choiceL [ parseFunction attrs; parseValue attrs ] "Binding"

    /// Parses a binding with `pBinding`, preceded by its optional attributes (pars.fsy
    /// `attr_localBinding`). The attributes follow the `let`, `let rec`, `and` or `with` keyword.
    let private attributed (pBinding: Attributes<SyntaxToken> voption -> FSParser<Binding<_>>) =
        parser {
            let! attrs = opt Attributes.parse
            return! pBinding attrs
        }

    /// Parses a binding preceded by its optional attributes.
    let parseAttributed: FSParser<Binding<_>> = attributed parse

    /// Parses one or more `and`-separated bindings using the provided binding parser, each preceded
    /// by its optional attributes.
    let sepByAnd1 (pBinding: Attributes<SyntaxToken> voption -> FSParser<Binding<_>>) =
        let pFirst = attributed pBinding

        let pAndBinding =
            parser {
                let! andTok = pAnd
                let! binding = attributed pBinding
                return struct (andTok, binding)
            }

        fun (reader: Reader<_, _, _>) ->
            match pFirst reader with
            | Error e -> Error e
            | Ok first ->
                match many pAndBinding reader with
                | Error e -> Error e
                | Ok rest ->
                    let bindings = ImmutableArray.CreateBuilder(1 + rest.Length)
                    let ands = ImmutableArray.CreateBuilder(rest.Length)
                    bindings.Add(first)

                    for i = 0 to rest.Length - 1 do
                        let struct (andTok, b) = rest.[i]
                        ands.Add(andTok)
                        bindings.Add(b)

                    preturn (struct (bindings.ToImmutable(), ands.ToImmutable())) reader

    let parseSepByAnd1: FSParser<_> = sepByAnd1 parse

[<AutoOpen>]
module private MemberHelpers =
    // Forward reference for MemberDefn to resolve circular dependencies
    let refMemberDefn = FSRefParser<MemberDefn<SyntaxToken>>()

[<RequireQualifiedAccess>]
module FieldInitializer =
    let parse: FSParser<FieldInitializer<SyntaxToken>> =
        parser {
            let! id = LongIdent.parse
            let! equals = pEquals
            let! expr = withContext OffsideContext.SeqBlock refExprInRecords.Parser
            return FieldInitializer(id, equals, expr)
        }

/// pars.fsy `atomicExprAfterType`: the argument after the type of `new T arg`, `inherit T arg`, an
/// object expression's `new T arg` and an attribute `[<T arg>]`.
[<RequireQualifiedAccess>]
module AtomicExprAfterType =
    let private errExpectedArgumentAfterType: ErrorType<PositionedToken, ParseState> =
        Message "Expected a constructor argument after the type"

    /// Whether `token` starts the argument: a constant, a string, or a parenthesised, braced or array
    /// expression.
    let private starts (token: Token) =
        match token with
        | Token.KWLParen
        | Token.KWLBrace
        | Token.KWLBraceBar
        | Token.KWLArrayBracket
        | Token.KWBegin
        | Token.KWNull
        | Token.KWTrue
        | Token.KWFalse
        | Token.StringOpen
        | Token.VerbatimStringOpen
        | Token.String3Open
        | Token.InterpolatedStringOpen
        | Token.VerbatimInterpolatedStringOpen
        | Token.Interpolated3StringOpen
        | Token.OpQuotationTypedLeft
        | Token.OpQuotationUntypedLeft -> true
        | _ -> TokenInfo.isLiteral token

    let parse (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextSyntaxToken reader with
        | Ok t when starts t.Token -> refExprAtomic.Parser reader
        | Ok _ -> fail errExpectedArgumentAfterType reader
        | Error e -> Error e

[<RequireQualifiedAccess>]
module ObjectConstruction =

    // Explicit initialization hook for static references
    let mutable private x = 0
    let init () = x <- x + 1

    let parse: FSParser<ObjectConstruction<SyntaxToken>> =
        parser {
            let! typ = Type.parse

            let! argExpr = opt AtomicExprAfterType.parse

            match argExpr with
            | ValueSome expr -> return ObjectConstruction.ObjectConstruction(typ, expr)
            | ValueNone -> return ObjectConstruction.InterfaceConstruction(typ)
        }

    do refObjectConstruction.Set parse

/// Auxiliary data passed from operator-specific parsers back to the expression parser for AST construction.
[<RequireQualifiedAccess>]
type ExprAux =
    | Ident of SyntaxToken
    | TypeApp of SyntaxToken * ImArr<Type<SyntaxToken>> * ImArr<SyntaxToken> * SyntaxToken
    | DotIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // .[ expr ]
    | DotParenOp of IdentOrOp<SyntaxToken> // .( op ) — qualified operator access
    | HighPrecApp of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // ( expr ) for f(x, y)
    | HighPrecIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // [ expr ] for arr[i] (F# 6+ dot-less indexing)
    | TypeCast of Type<SyntaxToken> // :> Type
    | KeywordOperand of Expr<SyntaxToken>
    /// A complete expression for a keyword prefix operator.
    | FinishedKeywordExpr of Expr<SyntaxToken>
    | SliceAll
    | SliceFrom
    | Range of Expr<SyntaxToken>

[<RequireQualifiedAccess>]
module Expr =
    let private errNoTrailingTypeAnnotation: ErrorType<PositionedToken, ParseState> =
        Message "no trailing type annotation"

    let private errExpectedInAtLetIndent: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'in' at the same indent as 'let'"

    let private errExpectedRhsOperator: ErrorType<PositionedToken, ParseState> =
        Message "Expected RHS operator"

    let private errUnexpectedBarOp: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected '|' operator in expression context"

    let private errUnexpectedDereferenceOp: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected dereference operator '!' in infix context"

    let private errUnexpectedArrowOp: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected arrow operator '->' in infix context"

    let private errUnexpectedPrefixKeywordRhs: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected prefix keyword in RHS expression context"

    let private errLhsOnlyOpInInfix: ErrorType<PositionedToken, ParseState> =
        Message "LHS-only operator cannot appear in infix position"

    let private errExpectedSliceAll: ErrorType<PositionedToken, ParseState> =
        Message "Expected ',' or ']' for slice all syntax"

    let private errNotPrefixOp: ErrorType<PositionedToken, ParseState> =
        Message "Not a prefix operator"

    let private errNextBlockItem: ErrorType<PositionedToken, ParseState> =
        Message "The next item of the block follows the inherit clause"

    let private errNotInheritRecord: ErrorType<PositionedToken, ParseState> =
        Message "Not an empty record or a record expression opening with 'inherit'"

    let private errNotCEBody: ErrorType<PositionedToken, ParseState> =
        Message "Not a computation expression body"

    let pl x : PrecedenceLevel = LanguagePrimitives.EnumOfValue x

    let private refExprRange = FSRefParser<Expr<SyntaxToken>>()

    /// A sequence block that ends before a `then` continuation: the operand of `yield` and `return`,
    /// whose `then` continues the enclosing block.
    let private refSeqBlockOperand = FSRefParser<Expr<SyntaxToken>>()

    // Parses `seqExpr [':' type]` with `pSeqBlock`, enforcing offside context on the type annotation.
    let private pTypedSeqExprBlockWith (pSeqBlock: FSParser<Expr<SyntaxToken>>) =
        let pColonPeek (reader: FSReader) =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.OpColon ->
                let col = ParseState.getIndent reader.State (reader.Index * 1<token>)

                match consumePeeked t reader with
                | Ok colon -> Ok(struct (colon, col))
                | Error e -> Error e
            | Ok _ -> fail errNoTrailingTypeAnnotation reader
            | Error e -> Error e

        parser {
            let! expr = pSeqBlock
            let! maybeColon = opt pColonPeek

            match maybeColon with
            | ValueSome(colon, colonCol) ->
                let! typ = withContextAt OffsideContext.Let (colonCol + 1) colon.PositionedToken Type.parse

                return Expr.TypeAnnotation(expr, colon, typ)
            | ValueNone -> return expr
        }

    let pTypedSeqExprBlock = pTypedSeqExprBlockWith refExprSeqBlock.Parser

    /// Functions that assemble AST Expr nodes from parsed operators and operands.
    module private Complete =
        let infix (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.InfixApp(l, op, r)

        let range (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) =
            match l with
            | Expr.Range(l, dotdot1, step) ->
                // Convert chained range `start..step..stop` into a stepped range
                Expr.SteppedRange(l, dotdot1, step, op, r)
            | _ -> Expr.Range(l, op, r)

        // Returns the complete expression parsed by a keyword body parser.
        let finishedKeyword _op (expr: ExprAux) =
            match expr with
            | ExprAux.FinishedKeywordExpr e -> e
            | _ -> failwith "Unexpected Aux type for finished keyword expression completion"

        let sequence (exprs: ResizeArray<Expr<_>>) ops =
            Expr.Sequential(ImmutableArray.CreateRange exprs, ImmutableArray.CreateRange ops)

        let assignment (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.Assignment(l, op, r)
        let prefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)
        let sliceTo (op: SyntaxToken) (e: Expr<_>) = Expr.SliceTo(op, e)

        let tuple (elements: ResizeArray<Expr<_>>) (ops: ResizeArray<SyntaxToken>) =
            Expr.Tuple(ImmutableArray.CreateRange elements, ImmutableArray.CreateRange ops)

        let app (elements: ResizeArray<Expr<_>>) ops =
            let args = ImmutableArray.CreateBuilder(elements.Count - 1)

            for i in 1 .. elements.Count - 1 do
                args.Add(elements[i])

            Expr.App(elements[0], args.ToImmutable())

        let sliceAll (op: SyntaxToken) (x: ExprAux) =
            match x with
            | ExprAux.SliceAll -> Expr.SliceAll(op)
            | _ -> failwith "Unexpected Aux type for SliceAll completion"

        let rangeOrSliceFrom (l: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.SliceFrom -> Expr.SliceFrom(l, op)
            | ExprAux.Range r -> range l op r
            | _ -> failwith "Unexpected Aux type for RangeOrSliceFrom completion"

        let dot (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.Ident ident ->
                match expr with
                | Expr.Ident firstIdent ->
                    let lid =
                        {
                            Idents = ImmutableArray.Create(firstIdent, ident)
                            Dots = ImmutableArray.Create(op)
                        }

                    Expr.LongIdentOrOp(LongIdentOrOp.LongIdent lid)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent existing) ->
                    let lid =
                        {
                            Idents = existing.Idents.Add(ident)
                            Dots = existing.Dots.Add(op)
                        }

                    Expr.LongIdentOrOp(LongIdentOrOp.LongIdent lid)
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.LongIdent(LongIdent.single ident))
            | ExprAux.DotParenOp identOrOp ->
                match expr with
                | Expr.Ident firstIdent ->
                    Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(LongIdent.single firstIdent, op, identOrOp))
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdent) ->
                    Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent, op, identOrOp))
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.Op identOrOp)
            | ExprAux.DotIndex(lBracket, indexExpr, rBracket) ->
                Expr.IndexedLookup(expr, ValueSome op, lBracket, indexExpr, rBracket)
            | _ -> failwith "Unexpected Aux type for dot completion"

        let dynamicLookup (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.Ident ident -> Expr.DynamicLookup(expr, op, ident)
            | _ -> failwith "Unexpected Aux type for dynamic lookup completion"

        let typeApp (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeApp(lAngle, types, commas, rAngle) -> Expr.TypeApp(expr, lAngle, types, commas, rAngle)
            | _ -> failwith "Unexpected Aux type for type application completion"

        let highPrec (funcExpr: Expr<_>) (_op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.HighPrecApp(lParen, argExpr, rParen) -> Expr.HighPrecedenceApp(funcExpr, lParen, argExpr, rParen)
            | ExprAux.HighPrecIndex(lBracket, argExpr, rBracket) ->
                Expr.IndexedLookup(funcExpr, ValueNone, lBracket, argExpr, rBracket)
            | _ -> failwith "Unexpected Aux type for high-precedence application/index completion"

        let typeCast (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeCast(typ) ->
                match op.Token with
                | Token.OpColon -> Expr.TypeAnnotation(expr, op, typ)
                | Token.OpUpcast -> Expr.StaticUpcast(expr, op, typ)
                | Token.OpDowncast -> Expr.DynamicDowncast(expr, op, typ)
                | Token.OpTypeTest -> Expr.DynamicTypeTest(expr, op, typ)
                | _ -> failwith "Unexpected operator for type cast completion"
            | _ -> failwith "Unexpected Aux type for type cast completion"

        let controlFlow (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.KeywordOperand e ->
                let kw =
                    match op.Token with
                    | Token.KWDo -> ControlFlowKeyword.Do op
                    | Token.KWDoBang -> ControlFlowKeyword.DoBang op
                    | Token.KWReturn -> ControlFlowKeyword.Return op
                    | Token.KWReturnBang -> ControlFlowKeyword.ReturnBang op
                    | Token.KWYield -> ControlFlowKeyword.Yield op
                    | Token.KWYieldBang -> ControlFlowKeyword.YieldBang op
                    | t -> failwith $"Unexpected keyword token for yield/return body {t}"

                Expr.ControlFlow(kw, e)
            | _ -> failwith "Unexpected Aux type for control-flow expression"

        let keywordPrefix (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.KeywordOperand e -> Expr.PrefixApp(op, e)
            | _ -> failwith "Unexpected Aux type for prefix keyword expression"

    /// Parsers for keyword-prefixed expressions (if, match, let, fun, etc.).
    /// Keywords are peeked by lhsParser and consumed here to establish offside indentation.
    module private KWBody =
        let recoverExprMissing p =
            recoverWith
                StoppingTokens.afterExpr
                DiagnosticCode.MissingExpression
                (missingOrSkipped Expr.Missing Expr.SkipsTokens)
                p

        let pIdentTok = nextSyntaxIdentifierLMsg "identifier"

        let pDoneVirt reader =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.KWDone -> consumePeeked t reader
            | Ok t ->
                let doneTok =
                    virtualToken (PositionedToken.Create(Token.VirtualDone, t.PositionedToken.StartIndex))

                Ok doneTok
            | Error e ->
                match reader.Peek() with
                | ValueSome t ->
                    let doneTok = virtualToken (PositionedToken.Create(Token.VirtualDone, t.StartIndex))
                    Ok doneTok
                | ValueNone -> failwith "Unexpected end of input while looking for 'done' or virtual 'done'"

        let pIfExpr =
            parser {
                let! (ifTok, indent) = assertKeywordToken Token.KWIf

                let! cond =
                    recoverExprMissing (
                        withContextAt OffsideContext.If (indent + 1) ifTok.PositionedToken refExpr.Parser
                    )

                let! thenTok = recoverWithVirtualToken Token.KWThen "Expected 'then' after condition" pThen
                // Grammar: THEN typedSeqExprBlock (anchored to if_col + 1)
                let! thenExpr =
                    recoverExprMissing (
                        withContextAt OffsideContext.Then (indent + 1) ifTok.PositionedToken refTypedSeqExprBlock.Parser
                    )

                let! elifs, elseBranch = ElifBranches.parse

                return ExprAux.FinishedKeywordExpr(Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, elifs, elseBranch))
            }

        let pMatchRules =
            withContext OffsideContext.MatchClauses Rules.parse
            |> recoverWith
                StoppingTokens.afterRule
                DiagnosticCode.MissingRule
                (fun toks ->
                    let missing =
                        Rule.Rule(
                            Pat.Missing,
                            ValueNone,
                            virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                            Expr.Missing
                        )

                    if toks.IsEmpty then
                        Rules(ValueNone, ImmutableArray.Create(missing), ImmutableArray.Empty)
                    else
                        let missingWithSkips =
                            Rule.Rule(
                                Pat.SkipsTokens(toks),
                                ValueNone,
                                virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                Expr.Missing
                            )

                        Rules(ValueNone, ImmutableArray.Create(missingWithSkips), ImmutableArray.Empty)
                )

        let pMatchExpr =
            let parseMatchBody
                (matchTok: SyntaxToken)
                (indent: int)
                (reader: Reader<PositionedToken, ParseState, _>)
                : Result<ExprAux, _> =
                let savedState = reader.State

                // Maintain Match context at match_col for 'with' and '|' alignment
                let matchEntry =
                    {
                        Context = OffsideContext.Match
                        Indent = indent
                        Token = matchTok.PositionedToken
                    }

                reader.State <- ParseState.pushOffside matchEntry reader.State

                match
                    recoverExprMissing
                        (withContextAt OffsideContext.SeqBlock (indent + 1) matchTok.PositionedToken refExpr.Parser)
                        reader
                with
                | Error e ->
                    reader.State <- savedState
                    Error e
                | Ok e ->

                    match
                        recoverWithVirtualToken Token.KWWith "Expected 'with' after match expression" pWith reader
                    with
                    | Error e ->
                        reader.State <- savedState
                        Error e
                    | Ok w ->

                        match pMatchRules reader with
                        | Error e ->
                            reader.State <- savedState
                            Error e
                        | Ok rules ->

                            reader.State <- ParseState.popOffside matchEntry reader.State
                            Ok(ExprAux.FinishedKeywordExpr(Expr.Match(matchTok, e, w, rules)))

            fun (reader: Reader<PositionedToken, ParseState, _>) ->
                match assertKeywordTokens Token.KWMatch Token.KWMatchBang reader with
                | Error e -> Error e
                | Ok(matchTok, indent) -> parseMatchBody matchTok indent reader

        let pFunctionExpr =
            parser {
                let! (funcTok, indent) = assertKeywordToken Token.KWFunction
                let! rules = withContextAt OffsideContext.Function indent funcTok.PositionedToken pMatchRules
                return ExprAux.FinishedKeywordExpr(Expr.Function(funcTok, rules))
            }

        let pFunExpr =
            // Grammar: FUN atomicPatterns RARROW typedSeqExprBlock
            let pBody = pTypedSeqExprBlock

            parser {
                let! (funTok, indent) = assertKeywordToken Token.KWFun

                let! result =
                    withContextAt
                        OffsideContext.Fun
                        (indent + 1)
                        funTok.PositionedToken
                        (parser {
                            // Lambda arguments use atomic patterns so identifiers are parsed as
                            // individual parameters rather than applied constructor patterns.
                            let! pats = Pat.parseAtomicBindingArgMany1

                            let! arrow =
                                recoverWithVirtualToken
                                    Token.OpArrowRight
                                    "Expected '->' after fun parameters"
                                    pArrowRight

                            let! expr = recoverExprMissing pBody
                            return Expr.Fun(funTok, pats, arrow, expr)
                        })

                return ExprAux.FinishedKeywordExpr result
            }

        let pTryExpr =
            let pTryMatchRules indent tryTok =
                withContextAt OffsideContext.MatchClauses indent tryTok Rules.parse
                |> recoverWith
                    StoppingTokens.afterRule
                    DiagnosticCode.MissingRule
                    (fun toks ->
                        let missing =
                            Rule.Rule(
                                Pat.Missing,
                                ValueNone,
                                virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                Expr.Missing
                            )

                        if toks.IsEmpty then
                            Rules(ValueNone, ImmutableArray.Create(missing), ImmutableArray.Empty)
                        else
                            let missingWithSkips =
                                Rule.Rule(
                                    Pat.SkipsTokens(toks),
                                    ValueNone,
                                    virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                    Expr.Missing
                                )

                            Rules(ValueNone, ImmutableArray.Create(missingWithSkips), ImmutableArray.Empty)
                    )

            // Explicitly manage Try context indentation across 'with' and 'finally' blocks
            fun (reader: Reader<PositionedToken, ParseState, _>) ->
                match assertKeywordToken Token.KWTry reader with
                | Error e -> Error e
                | Ok(tryTok, indent) ->

                    let savedState = reader.State

                    let tryEntry =
                        {
                            Context = OffsideContext.Try
                            Indent = indent
                            Token = tryTok.PositionedToken
                        }

                    reader.State <- ParseState.pushOffside tryEntry reader.State

                    // Grammar: TRY typedSeqExprBlock WITH|FINALLY ...
                    match recoverExprMissing refTypedSeqExprBlock.Parser reader with
                    | Error e ->
                        reader.State <- savedState
                        Error e
                    | Ok tryExpr ->

                        let pWith' =
                            parser {
                                let! withTok = pWith
                                let! rules = pTryMatchRules indent tryTok.PositionedToken
                                return Expr.TryWith(tryTok, tryExpr, withTok, rules)
                            }

                        let pFinally' =
                            parser {
                                let! finTok = pFinally
                                let! finExpr = refTypedSeqExprBlock.Parser
                                return Expr.TryFinally(tryTok, tryExpr, finTok, finExpr)
                            }

                        match
                            dispatchNextSyntaxTokenL
                                [ Token.KWWith, pWith'; Token.KWFinally, pFinally' ]
                                "Expected 'with' or 'finally'"
                                reader
                        with
                        | Error e ->
                            reader.State <- savedState
                            Error e
                        | Ok result ->
                            reader.State <- ParseState.popOffside tryEntry reader.State
                            Ok(ExprAux.FinishedKeywordExpr result)

        let pWhileExpr =
            // Accepts both `while` and `while!` (computation expression form)
            parser {
                let! (whileTok, indent) = assertKeywordTokens Token.KWWhile Token.KWWhileBang
                // Condition uses refExprNoSeq so 'do' is not consumed as a sequential expression
                let! cond =
                    recoverExprMissing (
                        withContextAt OffsideContext.While (indent + 1) whileTok.PositionedToken refExprNoSeq.Parser
                    )

                let! doTok =
                    recoverWithVirtualToken
                        Token.KWDo
                        "Expected 'do' after while condition"
                        (withContextAt OffsideContext.While (indent + 1) whileTok.PositionedToken pDo)
                // Grammar: WHILE _ DO typedSeqExprBlock
                let! body =
                    recoverExprMissing (
                        withContextAt OffsideContext.Do indent whileTok.PositionedToken refTypedSeqExprBlock.Parser
                    )

                let! doneTok = pDoneVirt
                return ExprAux.FinishedKeywordExpr(Expr.While(whileTok, cond, doTok, body, doneTok))
            }

        let pForExpr =
            let pForHeader =
                choiceL
                    [
                        // for ident = start to end do (also accepts _ as wildcard)
                        parser {
                            let! ident = pIdentTok <|> pWildcard
                            let! eq = pEquals
                            let! startExpr = refExprGuard.Parser
                            let! toTok = pToOrDownTo
                            let! endExpr = refExprGuard.Parser
                            let! doTok = pDo <|> pArrowRight

                            return
                                fun forTok body doneTok ->
                                    Expr.ForTo(forTok, ident, eq, startExpr, toTok, endExpr, doTok, body, doneTok)
                        }
                        // for pat in expr do
                        parser {
                            let! pat = Pat.parse
                            let! inTok = pIn
                            let! iterable = refExprGuard.Parser
                            let! doTok = pDo <|> pArrowRight

                            return
                                fun forTok body doneTok ->
                                    Expr.ForIn(forTok, pat, inTok, iterable, doTok, body, doneTok)
                        }
                    ]
                    "Expected 'for-to' or 'for-in' loop header"

            let assertFor reader =
                // Consumes 'for' keyword; throws if called unexpectedly
                match peekNextSyntaxToken reader with
                | Ok t when t.Token = Token.KWFor ->
                    (consumePeeked t
                     |>> fun forTok ->
                         let state = reader.State

                         let indent =
                             match forTok.Index with
                             | TokenIndex.Regular iT -> ParseState.getIndent state iT
                             | TokenIndex.Virtual ->
                                 invalidOp "Virtual tokens should not be used for 'for' keyword in pForBody"

                         struct (forTok, indent))
                        reader

                | Ok t -> invalidOp $"Expected 'for' keyword. Got {t.Token} at position {t.StartIndex}"
                | Error e -> invalidOp $"Expected 'for' keyword. Failed to peek next token: {e}"

            parser {
                let! (forTok, indent) = assertFor
                // Loop header requires deeper indent than 'for'; body can align with 'for'
                let headerMinIndent = indent + 1
                let bodyMinIndent = indent
                let! forBuilder = withContextAt OffsideContext.For headerMinIndent forTok.PositionedToken pForHeader
                // Grammar: FOR _ IN _ DO typedSeqExprBlock (and FOR _ = _ TO _ DO typedSeqExprBlock)
                let! body =
                    withContextAt OffsideContext.Do bodyMinIndent forTok.PositionedToken refTypedSeqExprBlock.Parser

                let! doneTok = pDoneVirt
                return ExprAux.FinishedKeywordExpr(forBuilder forTok body doneTok)
            }

        let pLetOrUseIn letIndent (reader: Reader<PositionedToken, ParseState, _>) =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.KWIn -> consumePeeked t reader
            | Ok t ->
                // Synthesize VirtualIn if indentation rules indicate the end of the binding definition
                let indent = ParseState.getIndent reader.State (reader.Index * 1<token>)
                let state = reader.State

                let atContextIndent =
                    match state.Context with
                    // Relaxed offside rule inside parentheses/brackets
                    | { Indent = 0 } :: _ -> true
                    | { Indent = ctxIndent } :: _ -> indent = ctxIndent || indent = letIndent
                    | [] -> indent = 0 || indent = letIndent

                if atContextIndent then
                    Ok(virtualToken (PositionedToken.Create(Token.VirtualIn, t.StartIndex)))
                else
                    fail errExpectedInAtLetIndent reader
            | Error _ ->
                // Synthesize VirtualIn at EOF to handle trailing `use` bindings at scope end
                let pos = reader.Index * 1<token>

                let startIndex =
                    if pos < reader.State.Lexed.Tokens.Length * 1<token> then
                        reader.State.Lexed.Tokens[pos].StartIndex
                    else
                        0

                Ok(virtualToken (PositionedToken.Create(Token.VirtualIn, startIndex)))

        // Parses `[rec] binding [and binding ...]`. Invalid forms like `use rec`
        // are accepted syntactically and flagged during semantic analysis.
        let pLetOrUseDefn indent token =
            withContextAt
                OffsideContext.Let
                indent
                token
                (parser {
                    let! recTok = opt pRec
                    let! bindings, ands = Binding.parseSepByAnd1
                    return struct (recTok, bindings, ands)
                })

        /// Folds a list of bindings into nested `Expr.LetOrUse` expressions (outermost first).
        let buildNestedLetOrUse
            (bindings: ResizeArray<struct (SyntaxToken * SyntaxToken voption * _ * _ * SyntaxToken)>)
            (body: Expr<SyntaxToken> voption)
            =
            let mutable result = body

            for i in bindings.Count - 1 .. -1 .. 0 do
                let struct (kwTok, recTok, defs, ands, inTok) = bindings[i]

                let kw =
                    match kwTok.Token with
                    | Token.KWLet -> LetOrUseKeyword.Let kwTok
                    | Token.KWUse -> LetOrUseKeyword.Use kwTok
                    | Token.KWLetBang -> LetOrUseKeyword.LetBang kwTok
                    | Token.KWUseBang -> LetOrUseKeyword.UseBang kwTok
                    | t -> failwith $"Unexpected keyword token for let/use body {t}"

                result <- ValueSome(Expr.LetOrUse(kw, recTok, defs, ands, ValueSome inTok, result))

            match result with
            | ValueSome expr -> expr
            | ValueNone -> Expr.Missing

        /// Parses sequential `let`/`use` bindings iteratively to prevent stack overflow from deep recursion.
        let pLetOrUseBody =
            fun (reader: Reader<PositionedToken, ParseState, _>) ->
                let bindings = ResizeArray()
                let mutable cont = true
                let mutable error = Unchecked.defaultof<ParseResult<ExprAux, _, _>>

                while cont do
                    match peekNextSyntaxToken reader with
                    | Error e ->
                        if bindings.Count = 0 then
                            error <- Error e

                        cont <- false
                    | Ok peeked ->
                        match peeked.Token with
                        | Token.KWLet
                        | Token.KWLetBang
                        | Token.KWUse
                        | Token.KWUseBang ->
                            match consumePeeked peeked reader with
                            | Error e ->
                                error <- Error e
                                cont <- false
                            | Ok kwTok ->
                                let indent =
                                    match kwTok.Index with
                                    | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                                    | TokenIndex.Virtual -> 0

                                match pLetOrUseDefn indent kwTok.PositionedToken reader with
                                | Error e ->
                                    error <- Error e
                                    cont <- false
                                | Ok(recTok, defs, ands) ->
                                    match pLetOrUseIn indent reader with
                                    | Error e ->
                                        error <- Error e
                                        cont <- false
                                    | Ok inTok -> bindings.Add(struct (kwTok, recTok, defs, ands, inTok))
                        | _ ->
                            if bindings.Count = 0 then
                                error <-
                                    Parsers.fail (Message $"Expected let/use keyword but got {peeked.Token}") reader

                            cont <- false

                if bindings.Count = 0 then
                    error
                else
                    // Grammar: LET bindings IN typedSeqExprBlock
                    let body =
                        match refTypedSeqExprBlock.Parser reader with
                        | Ok body -> ValueSome body
                        | Error err ->
                            let struct (lastKw, _, _, _, _) = bindings[bindings.Count - 1]

                            match lastKw.Token with
                            | Token.KWUse
                            | Token.KWUseBang ->
                                // Trailing `use`/`use!` with no subsequent body is valid F#
                                ValueNone
                            | _ ->
                                reader.State <-
                                    ParseState.addDiagnosticWithError
                                        DiagnosticCode.MissingExpression
                                        (diagnosticToken (peekNextSyntaxToken reader) reader)
                                        err
                                        reader.State

                                ValueSome Expr.Missing

                    let result = buildNestedLetOrUse bindings body
                    preturn (ExprAux.FinishedKeywordExpr result) reader

        /// The operand of `yield`, `return` or `do` and their bang forms, parsed by `pOperand`.
        let private pControlFlowBody (pOperand: FSParser<Expr<SyntaxToken>>) =
            let pBody =
                recoverWith
                    StoppingTokens.afterExpr
                    DiagnosticCode.MissingExpression
                    (missingOrSkipped Expr.Missing Expr.SkipsTokens)
                    pOperand

            parser {
                let! expr = pBody
                return ExprAux.KeywordOperand expr
            }

        // Grammar: YIELD|RETURN typedSeqExprBlock, whose `then` continues the enclosing block.
        let pYieldReturnBody =
            pControlFlowBody (pTypedSeqExprBlockWith refSeqBlockOperand.Parser)

        // Grammar: DO typedSeqExprBlock, whose `then` continues the operand.
        let pDoBody = pControlFlowBody refTypedSeqExprBlock.Parser

    /// High-precedence operators that bind tighter than whitespace application: `f(x)`, `f[i]`, and `f<T>`.
    module private HighPrec =
        // Cached error message to avoid allocations on failure paths
        let private pTypeApplicationErr: ErrorType<PositionedToken, ParseState> =
            Message "Expected '<' for type application"

        /// Lookahead parser for type applications (e.g. `<int>`).
        let pTypeApplication: FSParser<SyntaxToken> =
            fun reader ->
                if not (isPrevTokenSyntax reader) then
                    fail pTypeApplicationErr reader
                else
                    match reader.Peek() with
                    | ValueSome t when t.Token = Token.OpLessThan ->
                        // OpLessThan specifically identifies `<` without custom operator suffixes
                        let st = syntaxToken t reader.Index

                        let typeAngle =
                            { st with
                                PositionedToken = PositionedToken.Create(Token.VirtualTyApp, t.StartIndex)
                            }

                        preturn typeAngle reader
                    | _ -> fail pTypeApplicationErr reader

        let pTypeAppRhs =
            parser {
                let! lAngle = nextSyntaxTokenIsLMsg Token.OpLessThan "Expected '<' for type application"
                let! types, commas = sepBy Type.parse pComma

                let! state = getUserState

                let! rAngle = pCloseTypeParams

                return ExprAux.TypeApp(lAngle, types, commas, rAngle)
            }

        let pHighPrec token char =
            // Requires the token to be immediately adjacent to the preceding token (no whitespace)
            let errMsg = Message $"Expected '{char}' for high precedence"

            let pSatisfy =
                fun (reader: Reader<PositionedToken, ParseState, _>) ->
                    match reader.Peek() with
                    | ValueNone -> fail EndOfInput reader
                    | ValueSome t ->
                        if t.Token = token then
                            reader.Skip()
                            preturn t reader
                        else
                            fail errMsg reader

            let pFail (reader: Reader<PositionedToken, ParseState, _>) = fail errMsg reader

            parser {
                let! canBeHighPrec = isPrevTokenSyntax >> Ok

                if canBeHighPrec then

                    return! pSatisfy
                else
                    return! pFail
            }

        let pHighPrecLParen = pHighPrec Token.KWLParen '('

        let pHighPrecLBracket = pHighPrec Token.KWLBracket '['

        let peekHighPrecApp =
            lookAhead (choiceL [ pHighPrecLParen; pHighPrecLBracket ] "( or [ for high-precedence")
            |>> fun (pt: PositionedToken) ->
                match pt.Token with
                | Token.KWLParen -> virtualToken (PositionedToken.Create(Token.OpHighPrecedenceApp, pt.StartIndex))
                | Token.KWLBracket ->
                    virtualToken (PositionedToken.Create(Token.OpHighPrecedenceIndexApp, pt.StartIndex))
                | _ -> failwith "Unexpected token in peekHighPrecApp"

        let parseHighPrecIndex: FSParser<ExprAux> =
            // Parses dot-less indexing `arr[i]`
            parser {
                let! pos = getPosition
                let! lBracket = pHighPrecLBracket
                let lBracket = syntaxToken lBracket pos.Index
                // Special case: treat `f[]` as application to an empty list `f []`
                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWRBracket ->
                    let! rBracket = consumePeeked t
                    return ExprAux.HighPrecApp(lBracket, Expr.EmptyBlock(ParenKind.List lBracket, rBracket), rBracket)
                | _ ->
                    let! argExpr = refExpr.Parser
                    let! rBracket = pRBracket
                    return ExprAux.HighPrecIndex(lBracket, argExpr, rBracket)
            }

        let parseHighPrecApp: FSParser<ExprAux> =
            // Parses high-precedence application `f(x, y)`
            parser {
                let! pos = getPosition
                let! lParen = pHighPrecLParen
                let lParen = syntaxToken lParen pos.Index

                // Handle `f()` as application to unit
                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWRParen ->
                    let! rParen = consumePeeked t
                    return ExprAux.HighPrecApp(lParen, Expr.EmptyBlock(ParenKind.Paren lParen, rParen), rParen)
                | _ ->
                    let! argExpr = refExpr.Parser

                    let! argExpr =
                        parser {
                            match! peekNextSyntaxToken with
                            | t when t.Token = Token.OpColon ->
                                let! colon = consumePeeked t
                                let! typ = Type.parse
                                return Expr.TypeAnnotation(argExpr, colon, typ)
                            | _ -> return argExpr
                        }

                    let! rParen = pRParen
                    return ExprAux.HighPrecApp(lParen, argExpr, rParen)
            }

    module private Application =
        let parse =
            /// Checks if an operator has leading whitespace but no trailing whitespace (F# spec 3.8.1).
            let isAdjacentPrefixOp (state: ParseState) (rawIndex: int<token>) =
                let tokens = state.Lexed.Tokens

                let rightAdjacent =
                    rawIndex + 1<token> < tokens.Length * 1<token>
                    && not (ParseState.isTriviaToken state tokens[rawIndex + 1<token>])

                let leftSeparated =
                    rawIndex = 0<token>
                    || (rawIndex > 0<token>
                        && ParseState.isTriviaToken state tokens[rawIndex - 1<token>])

                rightAdjacent && leftSeparated

            let isAtomicExprToken (state: ParseState) (t: SyntaxToken) =
                match t.Token with
                | Token.Identifier
                | Token.BacktickedIdentifier
                | Token.UnterminatedBacktickedIdentifier
                | Token.KWNull
                // `base` is parsed syntactically as an identifier; invalid usages are caught semantically
                | Token.KWBase
                | Token.KWLParen
                | Token.KWLBracket
                | Token.KWLArrayBracket
                | Token.KWBegin
                | Token.KWStruct
                | Token.KWLBrace
                | Token.KWLBraceBar
                | Token.KWLHashParen
                | Token.OpQuotationTypedLeft
                | Token.OpQuotationUntypedLeft
                | Token.OpDereference
                | Token.InterpolatedStringOpen
                | Token.VerbatimInterpolatedStringOpen
                | Token.Interpolated3StringOpen
                | Token.StringOpen
                | Token.VerbatimStringOpen
                | Token.String3Open
                | Token.Wildcard -> true
                | _ ->
                    if Constant.isLiteralToken t.Token then
                        true
                    else
                        match OperatorInfo.TryCreate t.PositionedToken with
                        | ValueSome opInfo when opInfo.CanBePrefix ->
                            // Dual-use operators (e.g. `+`, `-`) only start arguments if right-adjacent
                            opInfo.Precedence = PrecedenceLevel.Prefix
                            || (
                                match t.Index with
                                | TokenIndex.Regular rawIndex -> isAdjacentPrefixOp state rawIndex
                                | TokenIndex.Virtual -> false
                            )
                        | _ -> false

            let failApp = fail (Message "Expected expression after application")

            // Parses function application (whitespace juxtaposition `f x`)
            parser {
                match! peekNextSyntaxToken with
                | t when t.Token = Token.EOF -> return! failApp
                | t ->
                    let! indent = currentIndent
                    let! state = getUserState

                    if not (isAtomicExprToken state t) then
                        return! failApp
                    else
                        let atAppIndent =
                            match state.Context with
                            | { Indent = ctxIndent } :: _ ->
                                if indent > ctxIndent then
                                    true
                                elif indent < ctxIndent then
                                    // Inside parentheses or brackets, undented tokens can still be valid
                                    // arguments if they are indented past the outer non-paren block.
                                    match state.Context with
                                    | { Context = OffsideContext.SeqBlock } :: ctx :: deeper when
                                        ctx.Context = OffsideContext.Paren
                                        || ctx.Context = OffsideContext.Bracket
                                        || ctx.Context = OffsideContext.BracketBar
                                        || ctx.Context = OffsideContext.BraceBar
                                        || ctx.Context = OffsideContext.Brace
                                        || ctx.Context = OffsideContext.Begin
                                        ->
                                        let rec walkPastParens stack =
                                            match stack with
                                            | [] -> indent > 0
                                            | (ctx: Offside) :: rest ->
                                                match ctx.Context with
                                                | OffsideContext.SeqBlock
                                                | OffsideContext.Paren
                                                | OffsideContext.Bracket
                                                | OffsideContext.BracketBar
                                                | OffsideContext.BraceBar
                                                | OffsideContext.Brace
                                                | OffsideContext.Begin -> walkPastParens rest
                                                | _ -> indent > ctx.Indent

                                        walkPastParens deeper
                                    | _ -> false
                                else
                                    false
                            | [] -> indent > 0

                        if atAppIndent then
                            return virtualToken (PositionedToken.Create(Token.VirtualApp, t.StartIndex))
                        else
                            return! failApp
            }

    /// Parses subsequent `,` delimiters in n-ary tuples.
    let private pTupleComma: Parser<SyntaxToken, _, _, _> =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpComma) "','"

    /// Parses explicit `;` or synthesizes `VirtualSep` for newline-separated sequential expressions.
    let private pSepVirt =
        let failSep =
            fail (Message "Expected ';' or newline at the same indent for expression sequencing")

        parser {
            match! peekNextSyntaxToken with
            | t when t.Token = Token.EOF -> return! failSep
            | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
            | t ->
                // Emit VirtualSep only if the next token can legally start an expression at this indent
                if TokenInfo.canStartExpression t.Token then
                    let! indent = currentIndent
                    let! state = getUserState

                    let atContextIndent =
                        match state.Context with
                        | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                        | [] -> indent = 0

                    // In module declarations, top-level `let`/`use` bindings at the module body column
                    // terminate any preceding expression sequence rather than being sequenced into it.
                    let isDeclStartAtBodyColumn =
                        match state.Context with
                        | frame :: _ when Offside.isDeclBlock frame ->
                            match t.Token with
                            | Token.KWLet
                            | Token.KWUse -> true
                            | _ -> false
                        | _ -> false

                    if atContextIndent then
                        if isDeclStartAtBodyColumn then
                            return! failSep
                        else
                            return virtualToken (PositionedToken.Create(Token.VirtualSep, t.StartIndex))
                    else
                        return! failSep
                else
                    return! failSep
        }

    type ExprOperatorParser() =
        let printOpInfo (op: OperatorInfo) =
            printfn
                $"Operator: {op.PositionedToken}({op.StartIndex}), Precedence: {op.Precedence}, Associativity: %A{op.Associativity}"

        let pIdentAfterDot = nextSyntaxIdentifierLMsg "Expected identifier after '.'"

        let parseDotRhs: FSParser<ExprAux> =
            choiceL
                [
                    // .[expr] — indexed access (e.g. xs.[0])
                    parser {
                        let! lBracket = pLBracket
                        let! indexExpr = refExpr.Parser
                        let! rBracket = pRBracket
                        return ExprAux.DotIndex(lBracket, indexExpr, rBracket)
                    }
                    // .(op) — qualified operator access (e.g. Module.(+))
                    parser {
                        let! lParen = pLParen
                        let! op = OpName.parse
                        let! rParen = pRParen
                        return ExprAux.DotParenOp(IdentOrOp.ParenOp(lParen, op, rParen))
                    }
                    // .ident — field/member access (e.g. x.Name)
                    parser {
                        let! ident = pIdentAfterDot
                        return ExprAux.Ident ident
                    }
                    // .N — positional union field access (e.g. expr.1)
                    parser {
                        let! intTok =
                            nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.NumInt32) "integer field index"

                        return ExprAux.Ident intTok
                    }
                ]
                "Dot RHS (identifier or index)"

        let parseRangeRhs reader =
            match peekNextSyntaxToken reader with
            | Ok t ->
                match t.Token with
                | Token.KWRBracket
                | Token.OpComma
                | Token.EOF ->
                    // Slices with open upper bounds (e.g. `arr[1..]`)
                    Ok ExprAux.SliceFrom
                | _ ->
                    // Full range expression on the RHS (e.g. `1..10`)
                    (refExprRange.Parser |>> ExprAux.Range) reader
            | Error e ->
                // EOF treated as an open upper bound
                Ok ExprAux.SliceFrom

        let parseTypeCastRhs = Type.parse |>> ExprAux.TypeCast

        // Parses RHS of dynamic member operator `?` (e.g. `obj?Property`)
        let parseDynamicRhs: FSParser<ExprAux> =
            parser {
                let! ident = nextSyntaxIdentifierLMsg "Expected identifier after '?'"
                return ExprAux.Ident ident
            }

        let dynamicLookupHandler: SyntaxToken -> RHSOperator<_, _, _, _, _, _> =
            let power = BindingPower.fromLevel (int PrecedenceLevel.Dot)
            fun op -> InfixMapped(op, preturn op, power, parseDynamicRhs, Complete.dynamicLookup)

        let rhsOperators
            : (SyntaxToken
                  -> RHSOperator<SyntaxToken, ExprAux, Expr<SyntaxToken>, PositionedToken, ParseState, FSReadable>) array =
            Array.init
                30
                (fun i ->
                    let pl = pl i
                    let power = BindingPower.fromLevel i

                    match pl with
                    | PrecedenceLevel.Pipe -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Semicolon -> (fun op -> InfixNary(op, pSepVirt, power, true, Complete.sequence))
                    | PrecedenceLevel.Assignment ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.assignment))
                    | PrecedenceLevel.Comma -> (fun op -> InfixNary(op, pTupleComma, power, false, Complete.tuple))
                    | PrecedenceLevel.Range ->
                        (fun op ->
                            // Binary `..` operator; disambiguated for ternary stepped ranges
                            InfixMapped(op, preturn op, power, parseRangeRhs, Complete.rangeOrSliceFrom)
                        )
                    | PrecedenceLevel.LogicalOr -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.LogicalAnd -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Cast ->
                        (fun op -> InfixMapped(op, preturn op, power, parseTypeCastRhs, Complete.typeCast))
                    | PrecedenceLevel.ComparisonAndBitwise ->
                        (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Append ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.Cons ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.TypeTest ->
                        (fun op -> InfixMapped(op, preturn op, power, parseTypeCastRhs, Complete.typeCast))
                    | PrecedenceLevel.InfixAdd -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.InfixMultiply -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Power ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.Application ->
                        (fun op -> InfixNary(op, Application.parse, power, false, Complete.app))
                    | PrecedenceLevel.Dot -> (fun op -> InfixMapped(op, preturn op, power, parseDotRhs, Complete.dot))
                    | PrecedenceLevel.HighIndexApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, HighPrec.parseHighPrecIndex, Complete.highPrec))
                    | PrecedenceLevel.HighApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, HighPrec.parseHighPrecApp, Complete.highPrec))
                    | PrecedenceLevel.HighTypeApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, HighPrec.pTypeAppRhs, Complete.typeApp))
                    | pl ->
                        (fun op ->
                            invalidOp
                                $"No operator handler for precedence level {pl}. Got op {op.Token} at position {op.StartIndex}"
                        )
                )

        let getRhsOperatorHandler (opInfo: OperatorInfo) token =
            // OpDynamic (`?`) shares Dot precedence but yields a different AST node
            match opInfo.Token with
            | Token.OpDynamic -> dynamicLookupHandler token
            | _ ->
                // Map token precedence level directly to RHS operator handler
                let pl = opInfo.Precedence
                let handler = rhsOperators[LanguagePrimitives.EnumToValue pl]
                handler token

        let rhsParser =
            let handleToken token =
                match OperatorInfo.TryCreate token.PositionedToken with
                | ValueNone -> fail errExpectedRhsOperator
                | ValueSome opInfo ->
                    match opInfo.Token with
                    | Token.OpBar -> fail errUnexpectedBarOp
                    | Token.OpDereference -> fail errUnexpectedDereferenceOp
                    | Token.OpArrowRight -> fail errUnexpectedArrowOp
                    | Token.KWLet
                    | Token.KWLetBang
                    | Token.KWUse
                    | Token.KWUseBang
                    | Token.KWMatch
                    | Token.KWMatchBang
                    | Token.KWDo
                    | Token.KWDoBang
                    | Token.KWReturn
                    | Token.KWReturnBang
                    | Token.KWYield
                    | Token.KWYieldBang
                    | Token.KWIf
                    | Token.KWFor
                    | Token.KWWhile
                    | Token.KWWhileBang
                    | Token.KWTry
                    | Token.KWFun
                    | Token.KWFunction -> fail errUnexpectedPrefixKeywordRhs
                    | _ when
                        // Reject LHS-only operators in infix position
                        (match opInfo.Precedence with
                         | PrecedenceLevel.Prefix
                         | PrecedenceLevel.Function
                         | PrecedenceLevel.If
                         | PrecedenceLevel.Let
                         | PrecedenceLevel.As
                         | PrecedenceLevel.When
                         | PrecedenceLevel.PatternMatchBar
                         | PrecedenceLevel.Parens -> true
                         | _ -> false)
                        ->
                        fail errLhsOnlyOpInInfix
                    | _ -> preturn (getRhsOperatorHandler opInfo token)

            // Dispatches peeked tokens to their corresponding RHS operator parsers.
            // Note: Token.OpLessThan represents all comparison tokens due to precedence bit-packing.
            let rhsDispatch: FSParser<SyntaxToken> =
                fun reader ->
                    match peekNextSyntaxToken reader with
                    | Error e -> Error e
                    | Ok token ->
                        let state = reader.State

                        match token.Token with
                        | Token.KWLParen
                        | Token.KWLBracket ->
                            match HighPrec.peekHighPrecApp reader with
                            | Ok r -> Ok r
                            | Error _ ->
                                match Application.parse reader with
                                | Ok r -> Ok r
                                | Error _ ->
                                    match pSepVirt reader with
                                    | Ok r -> Ok r
                                    | Error _ -> nextSyntaxToken reader

                        | Token.OpSemicolon -> pSepVirt reader

                        | Token.OpLessThan ->
                            // Check for type application syntax (e.g. `Foo<int>`)
                            match HighPrec.pTypeApplication reader with
                            | Ok r -> Ok r
                            | Error _ -> nextSyntaxToken reader

                        | _ when
                            // Re-lex fused operators (e.g. `>>`, `>=`) split during type argument parsing
                            state.CharsConsumedAfterTypeParams > 0 && TokenInfo.isOperator token.Token
                            ->
                            match reprocessedOperatorAfterTypeParams reader with
                            | Ok r -> Ok r
                            | Error _ ->
                                match Application.parse reader with
                                | Ok r -> Ok r
                                | Error _ ->
                                    match pSepVirt reader with
                                    | Ok r -> Ok r
                                    | Error _ -> nextSyntaxToken reader

                        | _ ->
                            match Application.parse reader with
                            | Ok r -> Ok r
                            | Error _ ->
                                match pSepVirt reader with
                                | Ok r -> Ok r
                                | Error _ -> nextSyntaxToken reader

            rhsDispatch >>= handleToken

        let peekIsSliceAll =
            parser {
                let! token = peekNextSyntaxToken
                // Checks if `*` is being used as a slice-all wildcard (`A[*, 1]`) rather than multiplication
                match token.Token with
                | Token.KWRBracket
                | Token.EOF
                | Token.OpComma -> return ExprAux.SliceAll
                | _ -> return! fail errExpectedSliceAll
            }

        // Dispatch helpers for prefix keyword operators
        let kwPrefixNoConsume body completer (token: SyntaxToken) =
            preturn (PrefixMapped(token, preturn token, body, completer))

        let kwPrefixConsume body completer (token: SyntaxToken) =
            parser {
                let! tok = consumePeeked token
                return PrefixMapped(tok, preturn tok, body, completer)
            }

        // Parses prefix expression keywords (`lazy`, `assert`, `fixed`, `upcast`, `downcast`)
        let pKeywordPrefixBody = refExprSeqBlock.Parser |>> ExprAux.KeywordOperand

        let pOpRangePrefix (token: SyntaxToken) =
            // A[..5]
            parser {
                let! tok = consumePeeked token
                let power = BindingPower.fromLevel (int PrecedenceLevel.Range)
                return Prefix(tok, preturn tok, power, Complete.sliceTo)
            }

        let pOpMultiplyPrefix (token: SyntaxToken) =
            // A[0..1,*] — `*` is slice-all only when followed by `,` / `]` / EOF
            parser {
                let! tok = consumePeeked token
                return PrefixMapped(tok, preturn tok, peekIsSliceAll, Complete.sliceAll)
            }

        // Address-of operators `&` and `&&` bind tighter than their infix logical variants
        let pOpAddressOfPrefix (token: SyntaxToken) =
            parser {
                let! tok = consumePeeked token
                let power = BindingPower.fromLevel (int PrecedenceLevel.Prefix)
                return Prefix(tok, preturn tok, power, Complete.prefix)
            }

        let kwPrefixRoutes: struct (Token * (SyntaxToken -> Parser<_, _, _, _>))[] =
            [|
                // CE and control-flow keyword prefixes — body parser consumes the keyword
                struct (Token.KWLet, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.finishedKeyword)
                struct (Token.KWMatch, kwPrefixNoConsume KWBody.pMatchExpr Complete.finishedKeyword)
                struct (Token.KWIf, kwPrefixNoConsume KWBody.pIfExpr Complete.finishedKeyword)
                struct (Token.KWFun, kwPrefixNoConsume KWBody.pFunExpr Complete.finishedKeyword)
                struct (Token.KWDo, kwPrefixConsume KWBody.pDoBody Complete.controlFlow)
                struct (Token.KWFor, kwPrefixNoConsume KWBody.pForExpr Complete.finishedKeyword)
                struct (Token.KWYieldBang, kwPrefixConsume KWBody.pYieldReturnBody Complete.controlFlow)
                struct (Token.KWYield, kwPrefixConsume KWBody.pYieldReturnBody Complete.controlFlow)
                struct (Token.KWReturn, kwPrefixConsume KWBody.pYieldReturnBody Complete.controlFlow)
                struct (Token.KWTry, kwPrefixNoConsume KWBody.pTryExpr Complete.finishedKeyword)
                struct (Token.KWFunction, kwPrefixNoConsume KWBody.pFunctionExpr Complete.finishedKeyword)
                struct (Token.KWUse, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.finishedKeyword)
                struct (Token.KWLetBang, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.finishedKeyword)
                struct (Token.KWDoBang, kwPrefixConsume KWBody.pDoBody Complete.controlFlow)
                struct (Token.KWWhile, kwPrefixNoConsume KWBody.pWhileExpr Complete.finishedKeyword)
                struct (Token.KWWhileBang, kwPrefixNoConsume KWBody.pWhileExpr Complete.finishedKeyword)
                struct (Token.KWReturnBang, kwPrefixConsume KWBody.pYieldReturnBody Complete.controlFlow)
                struct (Token.KWMatchBang, kwPrefixNoConsume KWBody.pMatchExpr Complete.finishedKeyword)
                struct (Token.KWUseBang, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.finishedKeyword)

                // Function-precedence keyword prefixes — body is a full typedSeqExprBlock
                struct (Token.KWLazy, kwPrefixConsume pKeywordPrefixBody Complete.keywordPrefix)
                struct (Token.KWAssert, kwPrefixConsume pKeywordPrefixBody Complete.keywordPrefix)
                struct (Token.KWFixed, kwPrefixConsume pKeywordPrefixBody Complete.keywordPrefix)
                struct (Token.KWUpcast, kwPrefixConsume pKeywordPrefixBody Complete.keywordPrefix)
                struct (Token.KWDowncast, kwPrefixConsume pKeywordPrefixBody Complete.keywordPrefix)

                // Operators with specialized prefix precedence or lookaheads
                struct (Token.OpRange, pOpRangePrefix)
                struct (Token.OpMultiply, pOpMultiplyPrefix)
                struct (Token.OpAmp, pOpAddressOfPrefix)
                struct (Token.OpAmpAmp, pOpAddressOfPrefix)
            |]

        // Fallback for prefix operators (`+`, `-`, `!`, etc.), binding tighter than their infix variants
        let pOperatorPrefix (token: SyntaxToken) =
            parser {
                match OperatorInfo.TryCreate(token.PositionedToken) with
                | ValueSome opInfo when opInfo.CanBePrefix ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int opInfo.Precedence) + 1uy<bp>
                    return Prefix(tok, preturn tok, power, Complete.prefix)
                | _ -> return! fail errNotPrefixOp
            }

        let lhsParser =
            parser {
                let! token = peekNextSyntaxToken

                let mutable handler = ValueNone
                let mutable i = 0

                while handler.IsNone && i < kwPrefixRoutes.Length do
                    let struct (tok, h) = kwPrefixRoutes.[i]

                    if token.Token = tok then
                        handler <- ValueSome h

                    i <- i + 1

                match handler with
                | ValueSome h -> return! h token
                | ValueNone -> return! pOperatorPrefix token
            }

        interface Operators<SyntaxToken, ExprAux, Expr<SyntaxToken>, PositionedToken, ParseState, FSReadable> with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser

    let pConst = Constant.parse |>> Expr.Const

    // Parses `null` literal expressions
    let pNullExpr = pNull |>> Expr.Null

    let private isStringTextFragment (tok: Token) =
        match tok with
        | Token.StringFragment
        | Token.EscapeSequence
        | Token.InterpolatedStringFragment
        | Token.VerbatimInterpolatedStringFragment
        | Token.Interpolated3StringFragment
        | Token.EscapeLBrace
        | Token.EscapeRBrace
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote -> true
        | _ -> false

    let private isStringInvalidText (tok: Token) =
        match tok with
        | Token.UnmatchedInterpolatedRBrace
        | Token.InvalidFormatPlaceholder
        | Token.InvalidFormatPercents
        | Token.TooManyLBracesInInterpolated3String
        | Token.TooManyRBracesInInterpolated3String -> true
        | _ -> false

    let private isStringClose (tok: Token) =
        match tok with
        | Token.StringClose
        | Token.ByteArrayClose
        | Token.VerbatimStringClose
        | Token.VerbatimByteArrayClose
        | Token.String3Close
        | Token.UnterminatedStringLiteral
        | Token.UnterminatedVerbatimStringLiteral
        | Token.UnterminatedString3Literal
        | Token.InterpolatedStringClose
        | Token.VerbatimInterpolatedStringClose
        | Token.Interpolated3StringClose
        | Token.UnterminatedInterpolatedString -> true
        | _ -> false

    let private stringKindOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringOpen -> StringKind.String t
        | Token.VerbatimStringOpen -> StringKind.VerbatimString t
        | Token.String3Open -> StringKind.String3 t
        | Token.InterpolatedStringOpen -> StringKind.InterpolatedString t
        | Token.VerbatimInterpolatedStringOpen -> StringKind.VerbatimInterpolatedString t
        | Token.Interpolated3StringOpen -> StringKind.Interpolated3String t
        | _ -> invalidOp $"Not a string open token: {t.Token}"

    let private isStringOpen (tok: Token) =
        match tok with
        | Token.StringOpen
        | Token.VerbatimStringOpen
        | Token.String3Open
        | Token.InterpolatedStringOpen
        | Token.VerbatimInterpolatedStringOpen
        | Token.Interpolated3StringOpen -> true
        | _ -> false

    let pString =
        let rec loop (isInterpolated: bool) (parts: ResizeArray<StringPart<SyntaxToken>>) reader =
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            // Text fragments (plain and interpolated)
            | Ok t when isStringTextFragment t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fragment ->
                    parts.Add(StringPart.Text fragment)
                    loop isInterpolated parts reader
            // Format specifier (%d etc.) — may be followed by expression hole in interpolated strings
            | Ok t when t.Token = Token.FormatPlaceholder ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok formatSpec ->
                    match peekNextSyntaxToken reader with
                    | Ok next when next.Token = Token.InterpolatedExpressionOpen ->
                        match consumePeeked next reader with
                        | Error e -> Error e
                        | Ok lBrace ->
                            match refExpr.Parser reader with
                            | Error e -> Error e
                            | Ok expr ->
                                let formatClause =
                                    match peekNextSyntaxToken reader with
                                    | Ok fc when fc.Token = Token.InterpolatedFormatClause ->
                                        consumePeeked fc reader |> ignore
                                        ValueSome fc
                                    | _ -> ValueNone

                                match nextSyntaxTokenIsLMsg Token.InterpolatedExpressionClose "Expected }" reader with
                                | Error e -> Error e
                                | Ok rBrace ->
                                    parts.Add(StringPart.Expr(ValueSome formatSpec, lBrace, expr, formatClause, rBrace))

                                    loop isInterpolated parts reader
                    | _ ->
                        if isInterpolated then
                            parts.Add(StringPart.OrphanFormatSpecifier formatSpec)
                        else
                            parts.Add(StringPart.FormatSpecifier formatSpec)

                        loop isInterpolated parts reader
            // Expression hole without format specifier (interpolated strings only)
            | Ok t when t.Token = Token.InterpolatedExpressionOpen ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok lBrace ->
                    match refExpr.Parser reader with
                    | Error e -> Error e
                    | Ok expr ->
                        let formatClause =
                            match peekNextSyntaxToken reader with
                            | Ok fc when fc.Token = Token.InterpolatedFormatClause ->
                                consumePeeked fc reader |> ignore
                                ValueSome fc
                            | _ -> ValueNone

                        match nextSyntaxTokenIsLMsg Token.InterpolatedExpressionClose "Expected }" reader with
                        | Error e -> Error e
                        | Ok rBrace ->
                            parts.Add(StringPart.Expr(ValueNone, lBrace, expr, formatClause, rBrace))
                            loop isInterpolated parts reader
            // Invalid text tokens (interpolated strings only)
            | Ok t when isStringInvalidText t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok invalid ->
                    parts.Add(StringPart.InvalidText invalid)
                    loop isInterpolated parts reader
            | Ok _ -> Ok parts

        fun (reader: Reader<PositionedToken, ParseState, _>) ->
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            // Plain strings are parsed with `parsePlainStringLiteral`; interpolated strings use the hole-aware loop above
            | Ok t when isPlainStringOpen t.Token ->
                match parsePlainStringLiteral "Expected string open" reader with
                | Error e -> Error e
                | Ok(kind, parts, closing) -> preturn (Expr.String(kind, parts, closing)) reader
            | Ok _ ->
                (parser {
                    let! opening = nextSyntaxTokenSatisfiesLMsg (fun t -> isStringOpen t.Token) "Expected string open"

                    let kind = stringKindOfToken opening

                    let isInterpolated =
                        match kind with
                        | StringKind.InterpolatedString _
                        | StringKind.VerbatimInterpolatedString _
                        | StringKind.Interpolated3String _ -> true
                        | _ -> false

                    let! parts = loop isInterpolated (ResizeArray())

                    let! closing = nextSyntaxTokenSatisfiesLMsg (fun t -> isStringClose t.Token) "Expected string close"

                    return Expr.String(kind, ImmutableArray.CreateRange parts, closing)
                })
                    reader

    let pIdentExpr = nextSyntaxIdentifierLMsg "Expected identifier" |>> Expr.Ident

    let private pEnclosed =
        let completeEmpty l r = Expr.EmptyBlock(l, r)
        let completeEnclosed l e r = Expr.EnclosedBlock(l, e, r)
        let skipsTokens toks = Expr.SkipsTokens(toks)
        pEnclosed completeEmpty completeEnclosed Expr.Missing skipsTokens

    let private pExprOrTypedPat =
        let pInnerExpr = refExprSeqBlock.Parser

        parser {
            let! expr = pInnerExpr

            match! peekNextSyntaxToken with
            | t when t.Token = Token.OpColon ->
                let! colon = consumePeeked t
                let! typ = Type.parse
                return Expr.TypeAnnotation(expr, colon, typ)
            | _ -> return expr
        }

    let private pParenOpExpr =
        parser {
            let! l = pLParen
            let! op = OpName.parse
            let! r = pRParen
            return Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(l, op, r)))
        }

    // SRTP trait call: (^T : (member Name : sig) expr)
    let private pStaticMemberInvocation =
        parser {
            let! lParen = pLParen

            let pBody =
                parser {
                    let! staticTypars = StaticTypars.parse
                    let! colon = pColon
                    let! lParenMember = pLParen
                    let! staticTok = opt pStatic
                    let! memberTok = pMember
                    let! membersig = Constraint.pConstraintMemberSig
                    let! rParenMember = pRParen
                    let! expr = refExprSeqBlock.Parser
                    let! rParen = pRParen

                    return
                        struct (staticTypars,
                                colon,
                                lParenMember,
                                staticTok,
                                memberTok,
                                membersig,
                                rParenMember,
                                expr,
                                rParen)
                }

            let! struct (staticTypars, colon, lParenMember, staticTok, memberTok, membersig, rParenMember, expr, rParen) =
                withContextAt OffsideContext.Paren 0 lParen.PositionedToken pBody

            return
                Expr.StaticMemberInvocation(
                    lParen,
                    staticTypars,
                    colon,
                    lParenMember,
                    staticTok,
                    memberTok,
                    membersig,
                    rParenMember,
                    expr,
                    rParen
                )
        }

    let pParen =
        choiceL
            [
                pStaticMemberInvocation
                pParenOpExpr
                pEnclosed
                    pLParen
                    Token.KWRParen
                    ParenKind.Paren
                    OffsideContext.Paren
                    DiagnosticCode.ExpectedRParen
                    pExprOrTypedPat
            ]
            "pParen"

    let pBeginEnd =
        pEnclosed
            pBegin
            Token.KWEnd
            ParenKind.BeginEnd
            OffsideContext.Begin
            DiagnosticCode.ExpectedEnd
            pTypedSeqExprBlock

    let pList =
        pEnclosed
            pLBracket
            Token.KWRBracket
            ParenKind.List
            OffsideContext.Bracket
            DiagnosticCode.ExpectedRBracket
            refExprSeqBlock.Parser

    let pArray =
        pEnclosed
            pLArrayBracket
            Token.KWRArrayBracket
            ParenKind.Array
            OffsideContext.BracketBar
            DiagnosticCode.ExpectedRArrayBracket
            refExprSeqBlock.Parser

    let pQuoteTyped =
        pEnclosed
            pQuotationTypedLeft
            Token.OpQuotationTypedRight
            ParenKind.Quoted
            OffsideContext.Quote
            DiagnosticCode.ExpectedQuotationTypedRight
            refExprSeqBlock.Parser

    let pQuoteUntyped =
        pEnclosed
            pQuotationUntypedLeft
            Token.OpQuotationUntypedRight
            ParenKind.DoubleQuoted
            OffsideContext.Quote
            DiagnosticCode.ExpectedQuotationUntypedRight
            refExprSeqBlock.Parser

    let pStructTuple =
        parser {
            let! kw = pStruct
            let! l = pLParen
            let! e = refExpr.Parser
            let! r = pRParen

            let struct (es, commas) =
                match e with
                | Expr.Tuple(es, commas) -> struct (es, commas)
                | e -> struct (ImmutableArray.Create(e), ImmutableArray.Empty)

            return Expr.StructTuple(kw, l, es, commas, r)
        }

    let pNewExpr =
        parser {
            let! newTok = pNew
            let! typ = Type.parse
            let! argExpr = AtomicExprAfterType.parse
            return Expr.New(newTok, typ, argExpr)
        }

    /// Consumes the closing delimiter for a record/anonymous record expression.
    /// Accepts the matching close token, or the mismatched one with a diagnostic.
    let private pRecordClose
        (openTok: SyntaxToken)
        (expectedClose: Token)
        (mismatchedClose: Token)
        : Parser<SyntaxToken, PositionedToken, ParseState, _> =
        fun reader ->
            match peekNextSyntaxToken reader with
            | Ok tok when tok.Token = expectedClose -> consumePeeked tok reader
            | Ok tok when tok.Token = mismatchedClose ->
                reader.State <-
                    ParseState.addDiagnosticAt
                        (DiagnosticCode.MismatchedDelimiter(openTok.Token, Site.ofToken openTok, expectedClose))
                        tok
                        reader.State

                consumePeeked tok reader
            | _ -> fail (Message $"Expected closing delimiter") reader

    /// Parses record field initializers with optional trailing semicolon and consumes the close delimiter.
    let private pRecordFieldsAndClose (openTok: SyntaxToken) (expectedClose: Token) (mismatchedClose: Token) =
        parser {
            let! fields, seps = withContext OffsideContext.SeqBlock (sepBy1 FieldInitializer.parse pRecordFieldSep)

            let! trailingSep = opt pSemi

            let seps =
                match trailingSep with
                | ValueSome sep -> seps.Add(sep)
                | ValueNone -> seps

            let! rClose = pRecordClose openTok expectedClose mismatchedClose
            return struct (fields, seps, rClose)
        }

    /// The argument of an `inherit` clause (pars.fsy `atomicExprAfterType`). A token at the column of
    /// the enclosing block starts the next item of the block instead.
    let parseInheritArg (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextSyntaxToken reader with
        | Ok t ->
            let atBlockColumn =
                match t.Index, reader.State.Context with
                | TokenIndex.Regular iT, { Indent = ctxIndent } :: _ -> ParseState.getIndent reader.State iT = ctxIndent
                | _ -> false

            match atBlockColumn with
            | true -> fail errNextBlockItem reader
            | false -> AtomicExprAfterType.parse reader
        | Error e -> Error e

    /// `inherit Base [arg] [sep]` opening a record expression (pars.fsy `recdExpr: INHERIT ...`).
    /// The separator is a `;` or a virtual one before a field initialiser on the next line.
    let private pRecordInherit =
        parser {
            let! inh = pInherit
            let! typ = Type.parse
            let! arg = opt parseInheritArg
            let! sep = opt pRecordFieldSep
            return RecordInherit(ClassInheritsDecl.ClassInheritsDecl(inh, typ, arg), sep)
        }

    /// Parses the field initialisers of a record literal opening with an `inherit` clause, which
    /// may have none, and consumes the close delimiter.
    let private pInheritRecordFieldsAndClose (openTok: SyntaxToken) =
        parser {
            let! inherits, (fields, seps) =
                withContext
                    OffsideContext.SeqBlock
                    (parser {
                        let! inherits = pRecordInherit

                        match inherits with
                        | RecordInherit(separator = ValueSome _) ->
                            let! fieldsAndSeps = sepBy FieldInitializer.parse pRecordFieldSep
                            return inherits, fieldsAndSeps
                        | RecordInherit(separator = ValueNone) ->
                            return inherits, (ImmutableArray.Empty, ImmutableArray.Empty)
                    })

            let! trailingSep = opt pSemi

            let seps =
                match trailingSep with
                | ValueSome sep -> seps.Add(sep)
                | ValueNone -> seps

            let! rClose = pRecordClose openTok Token.KWRBrace Token.KWRBraceBar
            return Expr.Record(ParenKind.Brace openTok, ValueSome inherits, fields, seps, rClose)
        }

    /// Parses the inner content of a record or anonymous record expression.
    let private pRecordInner
        (lParen: ParenKind<SyntaxToken>)
        (openTok: SyntaxToken)
        (expectedClose: Token)
        (mismatchedClose: Token)
        =
        choiceL
            [
                // { expr with Field = val; ... } — record clone/update
                parser {
                    let! baseExpr = refExprInRecords.Parser
                    let! withTok = pWith

                    let! struct (fields, seps, rClose) = pRecordFieldsAndClose openTok expectedClose mismatchedClose

                    return Expr.RecordClone(lParen, baseExpr, withTok, fields, seps, rClose)
                }
                // { Field = val; ... } — record literal
                parser {
                    let! struct (fields, seps, rClose) = pRecordFieldsAndClose openTok expectedClose mismatchedClose

                    return Expr.Record(lParen, ValueNone, fields, seps, rClose)
                }
            ]
            "Record fields"

    /// Shared helper: pushes offside context for braced expressions, runs inner parser, and restores state.
    let private pBracedExpr
        (pOpen: Parser<SyntaxToken, PositionedToken, ParseState, _>)
        (offsideCtx: OffsideContext)
        (innerParser: SyntaxToken -> Parser<Expr<SyntaxToken>, PositionedToken, ParseState, _>)
        (label: string)
        : Parser<_, PositionedToken, ParseState, _> =
        fun reader ->
            match pOpen reader with
            | Error e -> Error e
            | Ok openTok ->
                let savedState = reader.State

                let entry: Offside =
                    {
                        Context = offsideCtx
                        Indent = 0
                        Token = openTok.PositionedToken
                    }

                reader.State <- ParseState.pushOffside entry reader.State

                match (innerParser openTok) reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside entry reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail (Message label) reader

    let pRecordOrObjectExpr: Parser<_, PositionedToken, ParseState, _> =
        pBracedExpr
            pLBrace
            OffsideContext.Brace
            (fun lBrace ->
                choiceL
                    [
                        // Computation expression body identified by leading CE keyword
                        parser {
                            let! peekTok = peekNextSyntaxToken

                            match peekTok.Token with
                            | Token.KWLet
                            | Token.KWLetBang
                            | Token.KWUse
                            | Token.KWUseBang
                            | Token.KWDo
                            | Token.KWDoBang
                            | Token.KWYield
                            | Token.KWYieldBang
                            | Token.KWReturn
                            | Token.KWReturnBang
                            | Token.KWMatch
                            | Token.KWMatchBang
                            | Token.KWIf
                            | Token.KWTry
                            | Token.KWWhile
                            | Token.KWWhileBang
                            | Token.KWFor ->
                                let! body = refExprSeqBlock.Parser

                                let! rBrace = nextSyntaxTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                                return Expr.EnclosedBlock(ParenKind.Brace lBrace, body, rBrace)
                            | _ -> return! fail errNotCEBody
                        }
                        // Object expression: { new base-call with members ... }
                        parser {
                            let! newTok = pNew
                            let! construction = ObjectConstruction.parse

                            let! baseCall =
                                opt (
                                    parser {
                                        let! asTok = pAs
                                        let! ident = pIdent
                                        return struct (asTok, ident)
                                    }
                                )

                            let baseCall =
                                match baseCall with
                                | ValueSome struct (asTok, ident) -> BaseCall.NamedBaseCall(construction, asTok, ident)
                                | ValueNone -> BaseCall.AnonBaseCall(construction)

                            let! withTok = pWith
                            let! members = withContext OffsideContext.WithAugment (many refMemberDefn.Parser)

                            // Parse additional interface implementations
                            let! interfaceImpls =
                                many (
                                    parser {
                                        let! interfaceTok = pInterface
                                        let! typ = Type.parse
                                        let! intfWithTok = opt pWith

                                        match intfWithTok with
                                        | ValueSome wTok ->
                                            let! intfMembers =
                                                withContext OffsideContext.WithAugment (many refMemberDefn.Parser)

                                            let! intfNextTokOpt = opt peekNextSyntaxToken

                                            let intfVirtualEnd =
                                                {
                                                    PositionedToken =
                                                        mkVirtualPT
                                                            Token.KWEnd
                                                            (match intfNextTokOpt with
                                                             | ValueSome tok -> tok.StartIndex
                                                             | ValueNone -> 0)
                                                    Index = TokenIndex.Virtual
                                                }

                                            let objMembers =
                                                ObjectMembers.ObjectMembers(wTok, intfMembers, intfVirtualEnd)

                                            return InterfaceImpl.InterfaceImpl(interfaceTok, typ, ValueSome objMembers)
                                        | ValueNone -> return InterfaceImpl.InterfaceImpl(interfaceTok, typ, ValueNone)
                                    }
                                )

                            // Synthesize virtual end for the main ObjectMembers
                            let! nextTokOpt = opt peekNextSyntaxToken

                            let virtualEnd =
                                {
                                    PositionedToken =
                                        mkVirtualPT
                                            Token.KWEnd
                                            (match nextTokOpt with
                                             | ValueSome tok -> tok.StartIndex
                                             | ValueNone -> 0)
                                    Index = TokenIndex.Virtual
                                }

                            let objMembers = ObjectMembers.ObjectMembers(withTok, members, virtualEnd)

                            let! rBrace = nextSyntaxTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                            return Expr.Object(lBrace, ValueSome newTok, baseCall, objMembers, interfaceImpls, rBrace)
                        }
                        // `{}`, a record of no fields (pars.fsy `braceExpr`), and a record literal opening
                        // with `inherit Base(args)`
                        parser {
                            match! peekNextSyntaxToken with
                            | t when t.Token = Token.KWRBrace ->
                                let! rBrace = consumePeeked t
                                let fields = ImmutableArray<FieldInitializer<SyntaxToken>>.Empty

                                return
                                    Expr.Record(
                                        ParenKind.Brace lBrace,
                                        ValueNone,
                                        fields,
                                        ImmutableArray.Empty,
                                        rBrace
                                    )
                            | t when t.Token = Token.KWInherit -> return! pInheritRecordFieldsAndClose lBrace
                            | _ -> return! fail errNotInheritRecord
                        }
                        // Record literal or clone
                        pRecordInner (ParenKind.Brace lBrace) lBrace Token.KWRBrace Token.KWRBraceBar
                        // Fallback: computation expression without leading CE keyword (e.g. `seq { ... }`)
                        parser {
                            let! body = refExprSeqBlock.Parser

                            let! rBrace = nextSyntaxTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                            return Expr.EnclosedBlock(ParenKind.Brace lBrace, body, rBrace)
                        }
                    ]
                    "Record or RecordClone"
            )
            "Record or RecordClone"

    let pAnonRecordExpr: Parser<_, PositionedToken, ParseState, _> =
        pBracedExpr
            pLBraceBar
            OffsideContext.BraceBar
            (fun lBraceBar -> pRecordInner (ParenKind.BraceBar lBraceBar) lBraceBar Token.KWRBraceBar Token.KWRBrace)
            "Anonymous record"

    let private pILIntrinsic =
        // IL intrinsic: (# "instr" type('T) args : retType #)
        let pAnyToken = nextSyntaxTokenSatisfiesLMsg (fun _ -> true) "IL intrinsic token"

        let pTypeArg (reader: Reader<PositionedToken, ParseState, _>) =
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok tok when tok.Token = Token.KWType ->
                match consumePeeked tok reader with
                | Error e -> Error e
                | Ok typeKw ->
                    match nextSyntaxTokenIsLMsg Token.KWLParen "(" reader with
                    | Error e -> Error e
                    | Ok lParen ->
                        let tokens = ResizeArray()
                        let mutable depth = 1
                        let mutable rParen = Unchecked.defaultof<SyntaxToken>
                        let mutable error = ValueNone

                        while depth > 0 && error = ValueNone do
                            match pAnyToken reader with
                            | Error e -> error <- ValueSome e
                            | Ok t ->
                                if t.Token = Token.KWLParen then
                                    depth <- depth + 1
                                    tokens.Add(t)
                                elif t.Token = Token.KWRParen then
                                    depth <- depth - 1

                                    if depth > 0 then tokens.Add(t) else rParen <- t
                                else
                                    tokens.Add(t)

                        match error with
                        | ValueSome e -> Error e
                        | ValueNone ->
                            preturn
                                (ValueSome(ILTypeArg(typeKw, lParen, ImmutableArray.CreateRange tokens, rParen)))
                                reader
            | _ -> preturn ValueNone reader

        fun (reader: Reader<PositionedToken, ParseState, _>) ->
            match nextSyntaxTokenIsLMsg Token.KWLHashParen "(#" reader with
            | Error e -> Error e
            | Ok lHashParen ->
                match parsePlainStringLiteral "IL instruction string" reader with
                | Error e -> Error e
                | Ok(instrKind, instrParts, instrClose) ->
                    match pTypeArg reader with
                    | Error e -> Error e
                    | Ok typeArg ->
                        let args = ResizeArray()
                        let mutable finished = false
                        let mutable error = ValueNone

                        while not finished && error = ValueNone do
                            match peekNextSyntaxToken reader with
                            | Error e -> error <- ValueSome e
                            | Ok tok ->
                                match tok.Token with
                                | Token.KWRHashParen
                                | Token.OpColon -> finished <- true
                                | _ ->
                                    match refExprILArg.Parser reader with
                                    | Error e -> error <- ValueSome e
                                    | Ok arg -> args.Add(arg)

                        match error with
                        | ValueSome e -> Error e
                        | ValueNone ->
                            match opt ReturnType.parse reader with
                            | Error e -> Error e
                            | Ok returnType ->
                                match nextSyntaxTokenIsLMsg Token.KWRHashParen "#)" reader with
                                | Error e -> Error e
                                | Ok rHashParen ->
                                    preturn
                                        (Expr.ILIntrinsic(
                                            lHashParen,
                                            instrKind,
                                            instrParts,
                                            instrClose,
                                            typeArg,
                                            ImmutableArray.CreateRange args,
                                            returnType,
                                            rHashParen
                                        ))
                                        reader

    let private recoverExpr p =
        recoverWith
            StoppingTokens.afterExpr
            DiagnosticCode.MissingExpression
            (missingOrSkipped Expr.Missing Expr.SkipsTokens)
            p

    /// Parses optional argument expression in function calls (e.g. `?ident`).
    let private pOptionalArgExpr =
        parser {
            let! qmark = pQuestionMark
            let! ident = nextSyntaxIdentifierLMsg "Expected identifier after '?'"
            return Expr.OptionalArgExpr(qmark, ident)
        }

    let parseAtomic =
        dispatchNextSyntaxTokenFallback
            [
                Token.Identifier, pIdentExpr
                Token.KWLParen, recoverExpr pParen
                Token.StringOpen, pString
                Token.KWLBracket, recoverExpr pList
                Token.VerbatimStringOpen, pString
                Token.KWLBrace, recoverExpr pRecordOrObjectExpr
                Token.String3Open, pString
                Token.InterpolatedStringOpen, pString
                Token.KWLArrayBracket, recoverExpr pArray
                Token.KWNew, recoverExpr pNewExpr
                Token.KWLHashParen, pILIntrinsic
                Token.Wildcard, (nextSyntaxTokenIsLMsg Token.Wildcard "_" |>> Expr.Wildcard)
                Token.KWStruct, recoverExpr pStructTuple
                Token.KWLBraceBar, recoverExpr pAnonRecordExpr
                Token.VerbatimInterpolatedStringOpen, pString
                Token.Interpolated3StringOpen, pString
                Token.BacktickedIdentifier, pIdentExpr
                Token.OpDynamic, pOptionalArgExpr
                Token.KWBegin, recoverExpr pBeginEnd
                Token.OpQuotationTypedLeft, recoverExpr pQuoteTyped
                Token.OpQuotationUntypedLeft, recoverExpr pQuoteUntyped
                Token.KWBase, (nextSyntaxTokenIsLMsg Token.KWBase "base" |>> Expr.Ident)
                Token.UnterminatedBacktickedIdentifier, pIdentExpr
                Token.KWNull, pNullExpr
            ]
            pConst

    let operators = ExprOperatorParser()

    let parse = Operator.parser parseAtomic operators

    /// Whether the `then` token `thenTok` continues the sequence block on top of the context stack
    /// as `expr then thenExpr` (pars.fsy `declExpr THEN seqExpr`). A `then` offside of the block, or
    /// one closing the condition of an enclosing `if` or `elif`, ends the block.
    let private continuesWithThen (state: ParseState) (thenTok: SyntaxToken) =
        let rec outsideCondition (frames: Offside list) =
            match frames with
            | [] -> true
            | frame :: rest ->
                match frame.Context with
                | OffsideContext.If -> false
                | OffsideContext.Paren
                | OffsideContext.Bracket
                | OffsideContext.Brace
                | OffsideContext.BracketBar
                | OffsideContext.BraceBar
                | OffsideContext.Begin
                | OffsideContext.Quote -> true
                | _ -> outsideCondition rest

        match thenTok.Index, state.Context with
        | TokenIndex.Regular iT, block :: _ ->
            ParseState.getIndent state iT >= block.Indent && outsideCondition state.Context
        | _ -> false

    /// `expr then thenExpr`, where `expr` is the last item of a sequence: `a; b then c` is
    /// `a; (b then c)`, as in pars.fsy.
    let private sequentialThen (expr: Expr<SyntaxToken>) (thenTok: SyntaxToken) (thenExpr: Expr<SyntaxToken>) =
        match expr with
        | Expr.Sequential(exprs, seps) ->
            let last = exprs.Length - 1
            Expr.Sequential(exprs.SetItem(last, Expr.SequentialThen(exprs[last], thenTok, thenExpr)), seps)
        | _ -> Expr.SequentialThen(expr, thenTok, thenExpr)

    /// Parses an optional `then thenExpr` continuing `expr`.
    let private pThenContinuation (expr: Expr<SyntaxToken>) (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextSyntaxToken reader with
        | Ok t when t.Token = Token.KWThen && continuesWithThen reader.State t ->
            match consumePeeked t reader with
            | Ok thenTok ->
                match refTypedSeqExprBlock.Parser reader with
                | Ok thenExpr -> Ok(sequentialThen expr thenTok thenExpr)
                | Error e -> Error e
            | Error e -> Error e
        | _ -> Ok expr

    let parseSeqBlock =
        withContext OffsideContext.SeqBlock (parse >>= pThenContinuation)
        |> recoverWith
            StoppingTokens.afterExpr
            DiagnosticCode.MissingExpression
            (missingOrSkipped Expr.Missing Expr.SkipsTokens)

    do
        refExprAtomic.Set parseAtomic
        refExpr.Set parse
        refExprSeqBlock.Set parseSeqBlock
        refTypedSeqExprBlock.Set pTypedSeqExprBlock
        refSeqBlockOperand.Set(withContext OffsideContext.SeqBlock parse)

        // Non-sequential expressions (e.g. while conditions) where keywords like 'do' must not be consumed
        refExprNoSeq.Set(
            Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // IL intrinsic arguments: binds above Application to prevent whitespace juxtaposition
        refExprILArg.Set(
            Operator.parserAt (int PrecedenceLevel.Application + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // Expressions inside records and object expressions use higher precedence so `;` acts as a field separator
        refExprInRecords.Set(
            Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // Expressions in pattern guards (`when <expr>`) stop before `->`
        refExprGuard.Set(
            Operator.parserAt (int PrecedenceLevel.RArrow + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // Range expressions `start..end` or `start..step..end`
        refExprRange.Set(
            Operator.parserAt (int PrecedenceLevel.Range + 1 |> BindingPower.fromLevel) parseAtomic operators
        )
