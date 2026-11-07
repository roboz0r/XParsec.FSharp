namespace XParsec.FSharp.Parser

open XParsec
open XParsec.OperatorParsing
open XParsec.Parsers
open XParsec.FSharp.Lexer

/// Tracks the slice's starting offset so token indices can be mapped
/// back to their absolute positions in the source file.
[<Struct>]
type IfExprState = { AbsoluteStart: int }

[<RequireQualifiedAccess>]
module IfExpr =
    open SyntaxToken

    let private errNotValidLhsIfOp: ErrorType<PositionedToken, IfExprState> =
        Message "Not a valid LHS #if operator"

    let private errNotValidRhsIfOp: ErrorType<PositionedToken, IfExprState> =
        Message "Not a valid RHS #if operator"

    let private errNotValidIfTerm: ErrorType<PositionedToken, IfExprState> =
        Message "Not a valid #if term"

    let isTriviaToken (token: PositionedToken) =
        if token.InComment then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.LineComment
            | Token.Whitespace
            | Token.Tab -> true
            | _ -> false

    /// Skips whitespace and comments, returning the next significant token in the #if line.
    let rec nextSyntaxIfToken (reader: Reader<PositionedToken, IfExprState, 'Input>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token when isTriviaToken token ->
            reader.Skip()
            nextSyntaxIfToken reader
        | ValueSome token ->
            // Map the slice-relative index back to the absolute token array index.
            let absoluteIndex = reader.State.AbsoluteStart + reader.Index
            let t = syntaxToken token absoluteIndex
            reader.Skip()
            preturn t reader

    [<RequireQualifiedAccess>]
    type IfExprAux = unit

    /// Operator precedence parser for boolean expressions in `#if` directives (`!`, `&&`, `||`, and `()`).
    type IfExprParser<'Input when 'Input :> IReadable<PositionedToken, 'Input>>() =
        static let orPrecedence = BindingPower.fromLevel 1 // ||
        static let andPrecedence = BindingPower.fromLevel 2 // &&
        static let notPrecedence = BindingPower.fromLevel 3 // !
        static let parenPrecedence = BindingPower.fromLevel 4 // ( )

        // --- AST Node Builders ---

        static let completeInfix (l: IfExpr<SyntaxToken>) (op: SyntaxToken) (r: IfExpr<SyntaxToken>) =
            match op.Token with
            | Token.OpBarBar -> IfExpr.Or(l, op, r)
            | Token.OpAmpAmp -> IfExpr.And(l, op, r)
            | _ -> failwithf "Unexpected infix if operator: %A" op

        static let completePrefix (op: SyntaxToken) (e: IfExpr<SyntaxToken>) =
            match op.Token with
            | Token.OpDereference -> IfExpr.Not(op, e)
            | _ -> failwithf "Unexpected prefix if operator: %A" op

        static let completeParen (l: SyntaxToken) (m: IfExpr<SyntaxToken>) (r: SyntaxToken) = IfExpr.Paren(l, m, r)

        // --- Operator Parsers ---

        static let lhsParser: Parser<_, _, _, 'Input> =
            nextSyntaxIfToken
            >>= fun token ->
                match token.Token with
                | Token.OpDereference ->
                    // Prefix negation (`!FOO`)
                    let p = preturn token
                    let op = Prefix(token, p, notPrecedence, completePrefix)
                    preturn op
                | Token.KWLParen ->
                    // Grouping `( ... )`
                    let p = preturn token

                    let rParen =
                        virtualToken (PositionedToken.Create(Token.KWRParen, token.StartIndex + 1))

                    let op = Enclosed(token, p, parenPrecedence, rParen, p, completeParen)
                    preturn op
                | _ -> fail errNotValidLhsIfOp

        static let rhsParser: Parser<_, _, _, 'Input> =
            nextSyntaxIfToken
            >>= fun token ->
                match token.Token with
                | Token.OpBarBar ->
                    let p = preturn token
                    let op = InfixLeft(token, p, orPrecedence, completeInfix)
                    preturn op
                | Token.OpAmpAmp ->
                    let p = preturn token
                    let op = InfixLeft(token, p, andPrecedence, completeInfix)
                    preturn op
                | _ -> fail errNotValidRhsIfOp

        static let atomParser: Parser<_, _, _, 'Input> =
            nextSyntaxIfToken
            >>= fun token ->
                match token.Token with
                // In #if expressions, the lexer tags all symbols and keywords as Identifiers.
                | Token.Identifier -> preturn (IfExpr.Term token)
                | _ -> fail errNotValidIfTerm

        static let ifDirectiveParser: Parser<_, _, _, 'Input> =
            satisfyL (fun token -> token.Token = Token.IfDirective) "Not a #if directive"

        interface Operators<SyntaxToken, IfExprAux, IfExpr<SyntaxToken>, PositionedToken, IfExprState, 'Input> with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser

        static member AtomParser = atomParser
        static member IfDirectiveParser = ifDirectiveParser

    let parseSlice: Parser<_, _, _, _> =
        IfExprParser<FSReadable>.IfDirectiveParser
        >>. Operator.parser IfExprParser<FSReadable>.AtomParser (IfExprParser<FSReadable>())

    let evaluate (expr: IfExpr<SyntaxToken>) (isDefined: SyntaxToken -> bool) : bool =
        let rec eval e =
            match e with
            | IfExpr.Term t -> isDefined t
            | IfExpr.And(l, _, r) -> eval l && eval r
            | IfExpr.Or(l, _, r) -> eval l || eval r
            | IfExpr.Not(_, r) -> not (eval r)
            | IfExpr.Paren(_, m, _) -> eval m

        eval expr

    let evaluateStateful (expr: IfExpr<SyntaxToken>) (state: ParseState) : bool =
        let isDefined token = ParseState.isDefined state token
        evaluate expr isDefined
