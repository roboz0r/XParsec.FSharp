namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer


[<RequireQualifiedAccess>]
module AttributeTarget =
    let private pContextualKeyword s ctor =
        // Pre-allocate error message per target keyword at module load.
        let err: ErrorType<PositionedToken, ParseState> = Message(sprintf "Expected '%s'" s)

        parser {
            let! state = getUserState

            let! t =
                nextSyntaxTokenSatisfiesL
                    (fun t -> t.Token = Token.Identifier && ParseState.tokenStringIs s t state)
                    err

            return ctor t
        }

    let private pKw k ctor =
        nextSyntaxTokenIsLMsg k (sprintf "Expected '%A'" k) |>> ctor

    let parse: FSParser<AttributeTarget<SyntaxToken>> =
        choiceL
            [
                pContextualKeyword "assembly" AttributeTarget.Assembly
                pKw Token.KWModule AttributeTarget.Module
                pKw Token.KWReturn AttributeTarget.Return
                pContextualKeyword "field" AttributeTarget.Field
                pContextualKeyword "property" AttributeTarget.Property
                pContextualKeyword "param" AttributeTarget.Param
                pKw Token.KWType AttributeTarget.Type
                pContextualKeyword "constructor" AttributeTarget.Constructor
                pContextualKeyword "event" AttributeTarget.Event
            ]
            "AttributeTarget"

[<RequireQualifiedAccess>]
module Attribute =
    let parse: FSParser<Attribute<SyntaxToken>> =
        parser {
            // Target prefix is optional (e.g. "assembly:").
            // Without a colon, target keywords are treated as the attribute type name.
            let! target =
                opt (
                    parser {
                        let! t = AttributeTarget.parse
                        let! c = pColon
                        return (t, c)
                    }
                )

            let! construction = refObjectConstruction.Parser
            return Attribute.Attribute(target, construction)
        }

[<RequireQualifiedAccess>]
module AttributeSet =
    let private pLAttrBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWLAttrBracket) "Expected '[<'"

    let private pRAttrBracket =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWRAttrBracket) "Expected '>]'"

    let private pAttributeItem =
        parser {
            let! attr = Attribute.parse
            let! sep = opt pSemi
            return (attr, sep)
        }

    let private pAttributeItems = many pAttributeItem

    let parse: FSParser<AttributeSet<SyntaxToken>> =
        parser {
            let! lBracket = pLAttrBracket
            let! attributes = pAttributeItems
            let! rBracket = pRAttrBracket
            return AttributeSet(lBracket, attributes, rBracket)
        }

[<RequireQualifiedAccess>]
module Attributes =
    let parse: FSParser<Attributes<SyntaxToken>> = many1 AttributeSet.parse

[<RequireQualifiedAccess>]
module RangeOpName =
    // Matches '..' or step-range '(.. ..)'.
    // Parsed as two tokens to permit whitespace/comments between dots in definition syntax.
    let private pRangeFirst =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpRange) "Expected '..'"

    let private pRangeSecond =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpRange) "'..'"

    let parse: FSParser<RangeOpName<SyntaxToken>> =
        parser {
            let! first = pRangeFirst

            let! second = opt pRangeSecond

            match second with
            | ValueSome second -> return RangeOpName.DotDotDotDot(first, second)
            | ValueNone -> return RangeOpName.DotDot first
        }

[<RequireQualifiedAccess>]
module ActivePatternOpName =
    let private pIdent = nextSyntaxIdentifierLMsg "Expected identifier"

    let parse: FSParser<ActivePatternOpName<SyntaxToken>> =
        // Parses pattern cases: Case1 | Case2 | ... (with optional trailing wildcard '_')
        let rec parseSegments (builder: ImmutableArray<_>.Builder) =
            parser {
                let! ident = pIdent
                builder.Add(ident)

                let! bar = pBar

                match! opt (lookAhead pRParen) with
                | ValueSome _ -> return (builder.ToImmutable(), ValueNone, bar)
                | ValueNone ->
                    match! opt (lookAhead pWildcard) with
                    | ValueSome _ ->
                        let! underscore = nextSyntaxToken
                        let! finalBar = pBar
                        return (builder.ToImmutable(), ValueSome underscore, finalBar)
                    | ValueNone -> return! parseSegments builder
            }

        parser {
            // Parses the interior of (| Case1 | Case2 |). Outer parens handled by caller.
            let! lBar = pBar

            let! idents, underscore, rBar = parseSegments (ImmutableArray.CreateBuilder())

            return ActivePatternOp(lBar, idents, underscore, rBar)
        }

[<RequireQualifiedAccess>]
module OpName =
    let private pSymbolicOp =
        nextSyntaxTokenSatisfiesLMsg
            (fun t -> t.Token.IsOperator || TokenInfo.isOperatorKeyword t.Token)
            "Expected symbolic operator"
        |>> OpName.SymbolicOp

    // Matches '[]' as an operator name (e.g. `type List<'T> = | ([])`).
    let private pNilOp =
        parser {
            let! lBracket = pLBracket
            let! rBracket = pRBracket
            return OpName.NilOp(lBracket, rBracket)
        }

    let parse: FSParser<OpName<SyntaxToken>> =
        choiceL
            [
                RangeOpName.parse |>> OpName.RangeOp
                // Active patterns must be checked before symbolic ops since both start with '|'.
                ActivePatternOpName.parse |>> OpName.ActivePatternOp
                pNilOp
                pSymbolicOp
            ]
            "OpName"

[<RequireQualifiedAccess>]
module IdentOrOp =
    let private pIdentOrOpIdent = nextSyntaxIdentifierLMsg "Expected Identifier"

    let parse: FSParser<IdentOrOp<SyntaxToken>> =
        choiceL
            [
                pIdentOrOpIdent |>> IdentOrOp.Ident

                // Parenthesized operator, e.g. (+) or (|Even|Odd|)
                parser {
                    let! l = pLParen
                    let! op = OpName.parse
                    let! r = pRParen
                    return IdentOrOp.ParenOp(l, op, r)
                }
            ]
            "IdentOrOp"

[<RequireQualifiedAccess>]
module LongIdent =
    let private pIdent = nextSyntaxIdentifierLMsg "Expected Identifier"

    /// Creates a single-identifier LongIdent with no separator dots.
    let inline single (ident: 'T) : LongIdent<'T> =
        {
            Idents = ImmutableArray.Create(ident)
            Dots = ImmutableArray.Empty
        }

    // Custom loop preserves dot tokens for the CST while avoiding allocations for single identifiers.
    let parse: FSParser<LongIdent<SyntaxToken>> =
        fun reader ->
            match pIdent reader with
            | Error e -> Error e
            | Ok first ->
                let mutable idents = SmallArrayBuilder<_>()
                let mutable dots = SmallArrayBuilder<_>()
                idents.Add(first)
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match pDot reader with
                    | Ok dot ->
                        match pIdent reader with
                        | Ok ident ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)

                            dots.Add(dot)
                            idents.Add(ident)
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false

                Ok
                    {
                        Idents = idents.ToImmutable()
                        Dots = dots.ToImmutable()
                    }


[<RequireQualifiedAccess>]
module Access =

    let tokenOf (access: Access<'T>) : 'T =
        match access with
        | Access.Private t
        | Access.Internal t
        | Access.Public t -> t

    let private errExpectedAccess: ErrorType<PositionedToken, ParseState> =
        Message "Expected access modifier (private, public or internal)"

    let private failAccess = fail errExpectedAccess

    let ofToken (t: SyntaxToken) : Access<SyntaxToken> voption =
        match t.Token with
        | Token.KWPrivate -> ValueSome(Access.Private t)
        | Token.KWInternal -> ValueSome(Access.Internal t)
        | Token.KWPublic -> ValueSome(Access.Public t)
        | _ -> ValueNone

    let parse =
        parser {
            let! token = peekNextSyntaxToken

            match ofToken token with
            | ValueSome access ->
                let! _ = consumePeeked token
                return access
            | ValueNone -> return! failAccess
        }
