namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.ParseState


[<RequireQualifiedAccess>]
module Typar =

    let private pIdent =
        nextSyntaxIdentifierLMsg "Expected identifier for type parameter"

    let pAnon = pWildcard |>> Typar.Anon

    let pNamed =
        parser {
            let! quote = pSingleQuote
            // F# allows trivia (whitespace/comments) between the quote and identifier: `' (* comment *) T`
            let! ident = pIdent
            return Typar.Named(quote, ident)
        }

    let pStatic =
        parser {
            // Static type parameters (e.g. ^T) use the '^' operator token
            let! caret = pOpConcatenate
            let! ident = pIdent
            return Typar.Static(caret, ident)
        }

    let parse: FSParser<Typar<SyntaxToken>> =
        dispatchNextSyntaxTokenL
            [
                Token.Wildcard, pAnon
                Token.KWSingleQuote, pNamed
                // Handles 'T when lexed as a single TypeParameter token
                Token.TypeParameter, (nextSyntaxToken |>> fun tok -> Typar.Named(tok, tok))
                Token.OpConcatenate, pStatic
            ]
            "Typar"

[<RequireQualifiedAccess>]
module StaticTypars =
    // Parses multiple type parameters joined by 'or': ( ^a or ^b )
    let pOrList =
        parser {
            let! lParen = pLParen
            let! firstTypar = Typar.parse
            let! firstOr = pOr
            let! restTypars, moreOrs = sepBy Typar.parse pOr
            let! rParen = pRParen

            let allTypars =
                ImmutableArray.CreateRange(
                    seq {
                        yield firstTypar
                        yield! restTypars
                    }
                )

            let allOrs =
                ImmutableArray.CreateRange(
                    seq {
                        yield firstOr
                        yield! moreOrs
                    }
                )

            return StaticTypars.OrList(lParen, allTypars, allOrs, rParen)
        }

    // Parses a single type parameter (accepts both 'T and ^T)
    let pSingle = Typar.parse |>> StaticTypars.Single

    let parse = choiceL [ pOrList; pSingle ] "Static Type Parameters"

[<RequireQualifiedAccess>]
module WithClause =
    // Parses property accessor clauses: `with get`, `with set`, `with get, set`, or `with set, get`

    let private errExpectedGet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'get'"

    let private errExpectedSet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'set'"

    let private pGet =
        parser {
            let! getTok = nextSyntaxTokenIsLMsg Token.Identifier "get"
            let! state = getUserState

            if tokenStringIs "get" getTok state then
                return getTok
            else
                return! fail errExpectedGet
        }

    let private pSet =
        parser {
            let! setTok = nextSyntaxTokenIsLMsg Token.Identifier "set"
            let! state = getUserState

            if tokenStringIs "set" setTok state then
                return setTok
            else
                return! fail errExpectedSet
        }

    let private pGetSet =
        choiceL
            [
                parser {
                    let! getTok = pGet

                    let! maybeSet =
                        opt (
                            parser {
                                let! comma = pComma
                                let! setTok = pSet
                                return setTok
                            }
                        )

                    return getTok, maybeSet
                }
                parser {
                    let! setTok = pSet

                    let! maybeGet =
                        opt (
                            parser {
                                let! comma = pComma
                                let! getTok = pGet
                                return getTok
                            }
                        )

                    return setTok, maybeGet
                }
            ]
            ""

    /// Parses `with get` / `with set` / `with get,set` / `with set,get`.
    /// Returns struct (withToken, (firstAccessor, optional second accessor)).
    let parse =
        parser {
            let! withTok = pWith
            let! getSet = pGetSet
            return struct (withTok, getSet)
        }

[<RequireQualifiedAccess>]
module Constraint =

    let private errExpectedStructOrNullAfterNot: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'struct' or 'null' after 'not'"

    let private errUnknownParenConstraint: ErrorType<PositionedToken, ParseState> =
        Message "Unknown parenthesized constraint"

    let private errUnknownConstraintType: ErrorType<PositionedToken, ParseState> =
        Message "Unknown constraint type"

    // Pre-allocated static tokens and parsers
    let private pUnitIdent = nextSyntaxIdentifierLMsg "Expected 'unit'"

    let private pUpcast =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpUpcast) ":>"

    // Parses a member name in a constraint (identifier, operator, or active pattern)
    let private pConstraintMemberName: FSParser<IdentOrOp<SyntaxToken>> =
        IdentOrOp.parse

    // Parses a member constraint signature: ident ':' curried-sig ['with' get/set]
    let pConstraintMemberSig =
        parser {
            let! ident = pConstraintMemberName
            let! colon = pColon
            let! sign = refCurriedSig.Parser
            let! withClause = opt WithClause.parse

            match withClause with
            | ValueSome(withTok, getSet) -> return MemberSig.PropSig(ident, ValueNone, colon, sign, withTok, getSet)
            | ValueNone -> return MemberSig.MethodOrPropSig(ident, ValueNone, colon, sign)
        }

    // Parses static member traits: static-typars ':' '(' ['static'] 'member' member-sig ')'
    let private pMemberTrait =
        parser {
            let! staticTypars = StaticTypars.parse
            let! colon = pColon
            let! lParen = pLParen
            let! staticToken = opt pStatic
            let! memberToken = pMember
            let! membersig = pConstraintMemberSig
            let! rParen = pRParen
            return Constraint.MemberTrait(staticTypars, colon, lParen, staticToken, memberToken, membersig, rParen)
        }

    // Parses default constructor constraints: (new : unit -> 'T)
    let private pDefaultConstructor (typar: Typar<_>) colon lParen (tokenNew: SyntaxToken) =
        parser {
            let! colonUnit = pColon
            let! unitToken = pUnitIdent
            let! arrow = pArrowRight
            let! resultTypar = Typar.parse
            let! rParen = pRParen

            return
                Constraint.DefaultConstructor(
                    typar,
                    colon,
                    lParen,
                    tokenNew,
                    colonUnit,
                    unitToken,
                    arrow,
                    resultTypar,
                    rParen
                )
        }

    let private pTyparConstraints =
        parser {
            let! typar = Typar.parse

            // Check for coercion constraint (:>) vs standard constraint (:)
            let! coercion = opt pUpcast

            match coercion with
            | ValueSome op ->
                let! typ = refType.Parser
                return Constraint.Coercion(typar, op, typ)
            | ValueNone ->
                let! colon = pColon

                let! state = getUserState
                let! token = nextSyntaxToken

                match token.Token with
                | Token.KWNull -> return Constraint.Nullness(typar, colon, token)
                | Token.KWStruct -> return Constraint.Struct(typar, colon, token)
                | Token.KWDelegate ->
                    let! lAngle = pLessThan
                    let! t1 = refType.Parser
                    let! comma = pComma
                    let! t2 = refType.Parser
                    let! rAngle = pCloseTypeParams
                    return Constraint.Delegate(typar, colon, token, lAngle, t1, comma, t2, rAngle)
                | _ when tokenStringIs "equality" token state -> return Constraint.Equality(typar, colon, token)
                | _ when tokenStringIs "comparison" token state -> return Constraint.Comparison(typar, colon, token)
                | _ when tokenStringIs "unmanaged" token state -> return Constraint.Unmanaged(typar, colon, token)
                | _ when tokenStringIs "not" token state ->
                    let! next = nextSyntaxToken

                    match next.Token with
                    | Token.KWStruct -> return Constraint.ReferenceType(typar, colon, token, next)
                    | Token.KWNull -> return Constraint.NotNull(typar, colon, token, next)
                    | _ -> return! fail errExpectedStructOrNullAfterNot
                | _ when tokenStringIs "enum" token state ->
                    let! lAngle = pLessThan
                    let! t = refType.Parser
                    let! rAngle = pCloseTypeParams
                    return Constraint.Enum(typar, colon, token, lAngle, t, rAngle)
                | Token.KWLParen ->
                    let! next = nextSyntaxToken

                    if next.Token = Token.KWNew then
                        return! pDefaultConstructor typar colon token next
                    else
                        return! fail errUnknownParenConstraint
                | _ -> return! fail errUnknownConstraintType
        }

    let private pDefaultConstraint =
        parser {
            let! defaultTok = pDefault
            let! typar = Typar.parse
            let! colon = pColon
            let! typ = refType.Parser
            return Constraint.Default(defaultTok, typar, colon, typ)
        }

    let parse =
        choiceL [ pDefaultConstraint; pMemberTrait; pTyparConstraints ] "Constraint"

[<RequireQualifiedAccess>]
module TyparConstraints =
    /// Parses type parameter constraints: `when c1 and c2 ...`
    let parse: Parser<TyparConstraints<SyntaxToken>, _, _, _> =
        parser {
            let! whenTok = pWhen
            let! first = Constraint.parse

            let! rest =
                many (
                    parser {
                        let! andTok = pAnd
                        let! c = Constraint.parse
                        return struct (andTok, c)
                    }
                )

            return
                {
                    WhenToken = whenTok
                    Constraint = first
                    AndConstraints = rest
                }
        }

[<RequireQualifiedAccess>]
module TyparDefns =
    let parse =
        parser {
            let! lAngle = pLessThan

            let! defns, _ =
                sepBy1
                    (parser {
                        let! attrs = opt Attributes.parse
                        let! tp = Typar.parse
                        return TyparDefn.TyparDefn(attrs, tp)
                    })
                    pComma

            let! constraints = opt TyparConstraints.parse
            let! rAngle = pCloseTypeParams

            return TyparDefns.TyparDefns(lAngle, defns, constraints, rAngle)
        }

[<RequireQualifiedAccess>]
module Type =

    // Pre-allocated static parsers and errors
    let private pLHashParen = nextSyntaxTokenIsLMsg Token.KWLHashParen "(#"
    let private pRHashParen = nextSyntaxTokenIsLMsg Token.KWRHashParen "#)"
    let private pAnonRecFieldIdent = nextSyntaxIdentifierLMsg "field name"

    let private pUpcast =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpUpcast) ":>"

    let private errRetryAsMeasureArg: ErrorType<PositionedToken, ParseState> =
        Message "Retry as measure argument"

    // Parses a generic argument inside '<...>'.
    // Attempts to parse as a Type first; if trailing operators indicate a unit of measure
    // (e.g. '/', '*', '^'), backtracks and re-parses as a Measure.
    let private pTypeArg =
        choice
            [
                parser {
                    let! tNormal = refType.Parser
                    let! peekAfter = opt peekNextSyntaxToken
                    let! state = getUserState

                    match peekAfter with
                    | ValueNone -> return TypeArg.Type tNormal
                    | ValueSome peekAfter ->
                        let startsWithCaret = ParseState.tokenStringStartsWith "^" peekAfter state

                        if
                            peekAfter.Token = Token.OpDivision
                            || peekAfter.Token = Token.OpConcatenate
                            || peekAfter.Token = Token.OpMultiply
                            || startsWithCaret
                        then
                            return! fail errRetryAsMeasureArg
                        else
                            return TypeArg.Type tNormal
                }
                (refMeasure.Parser |>> TypeArg.Measure)
            ]

    // Parses a single field in an anonymous record: `ident : Type`
    let private pAnonRecField =
        parser {
            let! ident = pAnonRecFieldIdent
            let! colon = pColon
            let! typ = refType.Parser
            return AnonRecordField(ident, colon, typ)
        }

    let parseAtomic =
        choiceL
            [
                // Parenthesized type: (Type)
                parser {
                    let! l = pLParen
                    let! t = refType.Parser
                    let! r = pRParen
                    return Type.ParenType(l, t, r)
                }
                // Struct tuple: struct (Type * Type)
                parser {
                    let! s = pStruct
                    let! l = pLParen

                    let! ts, asterisks = sepBy refType.Parser pOpMultiply

                    let! r = pRParen
                    return Type.StructTupleType(s, l, ts, asterisks, r)
                }
                // Flexible type: #Type
                parser {
                    let! h = pHash
                    let! t = refType.Parser
                    return Type.AnonymousSubtype(h, t)
                }
                // Null literal type: null
                pNull |>> Type.Null
                // Type variable: 'a
                Typar.parse |>> Type.VarType
                // IL intrinsic types: (# "iltype" #) or (# class "iltype" #)
                parser {
                    let! l = pLHashParen

                    let! kindTag =
                        opt (
                            choiceL
                                [ pClass |>> ExternKind.Class; pInterface |>> ExternKind.Interface ]
                                "external class/interface tag"
                        )

                    let! (kind, parts, instrClose) = parsePlainStringLiteral "IL instruction string"
                    let! r = pRHashParen
                    return Type.ILIntrinsic(l, kindTag, kind, parts, instrClose, r)
                }
                // Anonymous record: {| x: int; y: string |}
                parser {
                    let! lBraceBar = pLBraceBar
                    let! fields, seps = sepBy1 pAnonRecField pSemi
                    let! rBraceBar = pRBraceBar
                    return Type.AnonRecordType(lBraceBar, fields, seps, rBraceBar)
                }
                // Named type or Generic type: LongIdent or LongIdent<Type, ...>
                parser {
                    let! lid = LongIdent.parse
                    // Once '<' is encountered, commit to parsing generic arguments
                    let! lessOpt = opt pLessThan

                    match lessOpt with
                    | ValueNone -> return Type.NamedType(lid)
                    | ValueSome l ->
                        let! args, commas = sepBy pTypeArg pComma
                        let! r = pCloseTypeParams
                        return Type.GenericType(lid, l, args, commas, r)
                }
            ]
            "Atomic Type"

    let private pArrayOpenBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWLArrayBracket || t.Token = Token.KWLBracket) "["

    let private pArrayCloseBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWRArrayBracket || t.Token = Token.KWRBracket) "]"

    let private errExpectedDot: ErrorType<PositionedToken, ParseState> =
        Message "Expected '.'"

    // Parses '.' as a standard token or extracted from a fused operator (e.g. '>.' after type arguments)
    let private pPostfixDot =
        choiceL
            [
                pDot
                parser {
                    let! reprocessed = reprocessedOperatorAfterTypeParams

                    if reprocessed.PositionedToken.Token = Token.OpDot then
                        return reprocessed
                    else
                        return! fail errExpectedDot
                }
            ]
            "."

    // Postfix array rank continuation: `[]`, `[,]`, or `[|]`
    let private pPostfixArrayCont =
        parser {
            let! l = pArrayOpenBracket
            let! commas = many pComma
            let! r = pArrayCloseBracket
            return struct (l, commas, r)
        }

    // Postfix dotted access continuation: `.NestedType`
    let private pPostfixDottedCont =
        parser {
            let! dot = pPostfixDot
            let! lid = LongIdent.parse
            return struct (dot, lid)
        }

    // Postfix type suffix continuation: `int list`
    let private pPostfixSuffixCont: FSParser<LongIdent<SyntaxToken>> = LongIdent.parse

    // Parses postfix type modifiers:
    //   - Array indexing: `int[]`, `int[,]`
    //   - Dotted members: `Foo<int>.Bar`
    //   - Type suffixes:  `int list`
    let private pPostfixType: FSParser<Type<SyntaxToken>> =
        fun reader ->
            match parseAtomic reader with
            | Error e -> Error e
            | Ok atom ->
                let mutable acc = atom
                let mutable errOpt = ValueNone
                let mutable keepGoing = true

                while keepGoing && errOpt.IsNone do
                    match peekNextSyntaxToken reader with
                    | Error _ -> keepGoing <- false
                    | Ok peeked ->
                        let state = reader.State

                        // Handle fused operator boundaries after generic closing bracket (e.g. `Foo<int>.Bar`)
                        if state.CharsConsumedAfterTypeParams > 0 && TokenInfo.isOperator peeked.Token then
                            match opt pPostfixDottedCont reader with
                            | Ok(ValueSome(struct (dot, lid))) -> acc <- Type.DottedType(acc, dot, lid)
                            | Ok ValueNone -> keepGoing <- false
                            | Error e -> errOpt <- ValueSome e
                        else
                            match peeked.Token with
                            | Token.KWLBracket
                            | Token.KWLArrayBracket ->
                                match pPostfixArrayCont reader with
                                | Ok(struct (l, commas, r)) -> acc <- Type.ArrayType(acc, l, commas, r)
                                | Error e -> errOpt <- ValueSome e
                            | Token.OpDot ->
                                match pPostfixDottedCont reader with
                                | Ok(struct (dot, lid)) -> acc <- Type.DottedType(acc, dot, lid)
                                | Error e -> errOpt <- ValueSome e
                            | Token.Identifier
                            | Token.BacktickedIdentifier
                            | Token.UnterminatedBacktickedIdentifier ->
                                // Ensure postfix suffix (e.g. `list` in `int list`) respects indentation rules
                                // so it does not swallow identifiers on subsequent lines.
                                let peekedCol =
                                    match peeked.Index with
                                    | TokenIndex.Virtual -> 0
                                    | TokenIndex.Regular ti -> ParseState.getIndent state ti

                                let ctxIndent =
                                    match state.Context with
                                    | innermost :: _ -> innermost.Indent
                                    | [] -> 0

                                if peekedCol > ctxIndent then
                                    match pPostfixSuffixCont reader with
                                    | Ok lid -> acc <- Type.SuffixedType(acc, lid)
                                    | Error e -> errOpt <- ValueSome e
                                else
                                    keepGoing <- false
                            | _ -> keepGoing <- false

                match errOpt with
                | ValueSome e -> Error e
                | ValueNone -> Ok acc

    // Flexible type annotation / subtype constraint: `'T :> IDisposable`
    let private pSubtypeType =
        parser {
            let! lhs = pPostfixType

            match lhs with
            | Type.VarType typar ->
                let! colonGreater = opt pUpcast

                match colonGreater with
                | ValueSome op ->
                    let! rhs = pPostfixType
                    return Type.SubtypeConstraint(typar, op, rhs)
                | ValueNone -> return lhs
            | _ -> return lhs
        }

    let private pBarType =
        parser {
            let! bar = pBar
            let! rhs = pSubtypeType
            return (bar, rhs)
        }

    // Left-associative type union (e.g. nullable reference types: `string | null`)
    let private pUnionType =
        parser {
            let! lhs = pSubtypeType
            let mutable acc = lhs
            let mutable keepGoing = true

            while keepGoing do
                match! opt pBarType with
                | ValueSome(bar, rhs) -> acc <- Type.UnionType(acc, bar, rhs)
                | ValueNone -> keepGoing <- false

            return acc
        }

    // Tuple types: `T * T * T`
    let private pTupleType =
        parser {
            let! types, asterisks = sepBy1 pUnionType pOpMultiply

            if types.Length = 1 then
                return types[0]
            else
                return Type.TupleType(types, asterisks)
        }

    // Right-associative function types: `T -> T -> T`
    let private pFunctionType =
        parser {
            let! first = pTupleType
            let mutable current = first
            let mutable pairs: struct (Type<SyntaxToken> * SyntaxToken) list = []
            let mutable keepGoing = true

            while keepGoing do
                match! opt pArrowRight with
                | ValueSome arr ->
                    let! next = pTupleType
                    pairs <- struct (current, arr) :: pairs
                    current <- next
                | ValueNone -> keepGoing <- false

            let mutable acc = current

            for p in pairs do
                let struct (t, arr) = p
                acc <- Type.FunctionType(t, arr, acc)

            return acc
        }

    /// Entry point for parsing types, including optional `when` constraints.
    let parse =
        parser {
            let! typ = pFunctionType

            match! opt TyparConstraints.parse with
            | ValueSome constraints -> return Type.WhenConstrainedType(typ, constraints)
            | ValueNone -> return typ
        }

    /// Parses a single type without consuming `*` as a tuple separator (e.g. for DU case fields).
    let parseField = pPostfixType

    /// Parses an argument type where `*` separates arguments but `|` is allowed for nullable types (`T | null`).
    let parseArgField = pUnionType

    // Parsers that do not treat `|` as a type union (used where `|` serves as a syntax delimiter, like DU cases)
    let private pTupleTypeNoUnion =
        parser {
            let! types, asterisks = sepBy1 pSubtypeType pOpMultiply

            if types.Length = 1 then
                return types[0]
            else
                return Type.TupleType(types, asterisks)
        }

    let private pFunctionTypeNoUnion =
        parser {
            let! first = pTupleTypeNoUnion
            let mutable current = first
            let mutable pairs: struct (Type<SyntaxToken> * SyntaxToken) list = []
            let mutable keepGoing = true

            while keepGoing do
                match! opt pArrowRight with
                | ValueSome arr ->
                    let! next = pTupleTypeNoUnion
                    pairs <- struct (current, arr) :: pairs
                    current <- next
                | ValueNone -> keepGoing <- false

            let mutable acc = current

            for p in pairs do
                let struct (t, arr) = p
                acc <- Type.FunctionType(t, arr, acc)

            return acc
        }

    /// Parses a type without treating `|` as a type-union separator.
    let parseNoUnion =
        parser {
            let! typ = pFunctionTypeNoUnion

            match! opt TyparConstraints.parse with
            | ValueSome constraints -> return Type.WhenConstrainedType(typ, constraints)
            | ValueNone -> return typ
        }

    // Initialize the recursive type parser reference
    do refType.Set parse

module ReturnType =
    let parse: FSParser<ReturnType<_>> =
        parser {
            let! colon = pColon

            let! typ =
                Type.parse
                |> recoverWith
                    StoppingTokens.afterType
                    DiagnosticCode.MissingType
                    (missingOrSkipped Type<SyntaxToken>.Missing Type<SyntaxToken>.SkipsTokens)

            return ReturnType(colon, typ)
        }
