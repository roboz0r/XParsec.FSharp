namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken

[<RequireQualifiedAccess>]
module Constant =
    let private errExpectedGtMeasure: ErrorType<PositionedToken, ParseState> =
        Message "Expected '>' for measure"

    let private errExpectedLtMeasure: ErrorType<PositionedToken, ParseState> =
        Message "Expected '<' for measure"

    let isLiteralToken (t: Token) =
        t.IsNumeric || t = Token.KWTrue || t = Token.KWFalse || t = Token.CharLiteral

    let private pLiteral: FSParser<_> =
        nextSyntaxTokenSatisfiesLMsg
            (fun (synTok: SyntaxToken) -> isLiteralToken synTok.Token)
            "Expected constant literal"

    /// Parses the closing '>' for a measure.
    ///
    /// Handles indexer boundaries (e.g. `arr.[0<m>]`) where `>` and `]` were lexed
    /// together as `>]` (KWRAttrBracket). In that case, returns a virtual `>` and
    /// sets a state flag so the next read yields the trailing `]`.
    let private pMeasureRAngle =
        parser {
            match! peekNextSyntaxToken with
            | t when t.Token = Token.OpGreaterThan -> return! consumePeeked t
            | t when t.Token = Token.KWRAttrBracket ->
                do!
                    updateUserState (fun s ->
                        ParseState.ifTrace s (fun tc -> tc.SplitRAttrBracketSet(t.StartIndex))
                        { s with SplitRAttrBracket = true }
                    )

                return virtualToken (PositionedToken.Create(Token.OpGreaterThan, t.StartIndex))
            | _ -> return! fail errExpectedGtMeasure
        }

    /// Parses a measure suffix on a literal (e.g. `1.0<kg>`).
    ///
    /// F# requires the opening `<` to be immediately adjacent to the numeric
    /// literal with no intervening whitespace or comments.
    let private pMeasure =
        parser {
            let! canBeMeasure = isPrevTokenSyntax >> Ok

            if canBeMeasure then
                let! pos = getPosition

                let! lAngle =
                    satisfyL (fun (t: PositionedToken) -> t.Token = Token.OpLessThan) "Expected '<' for measure"

                let! m = Measure.parse
                let! rAngle = pMeasureRAngle

                return struct (syntaxToken lAngle pos.Index, m, rAngle)
            else
                return! fail errExpectedLtMeasure
        }

    /// Collects all type variables (e.g. `'u`) in a measure expression.
    let collectTypars (measure: Measure<'T>) : ResizeArray<Typar<'T>> =
        let results = ResizeArray()

        let rec visit m =
            match m with
            | Measure.Typar token -> results.Add(token)
            | Measure.Named _
            | Measure.One _
            | Measure.Anonymous _ -> ()
            | Measure.Paren(_, inner, _)
            | Measure.Reciprocal(_, inner)
            | Measure.Power(inner, _, _, _) -> visit inner
            | Measure.Product(l, _, r)
            | Measure.Quotient(l, _, r) ->
                visit l
                visit r
            | Measure.Juxtaposition(ms, ops) ->
                for m in ms do
                    visit m

        visit measure
        results

    /// Ensures literal measures (e.g. `1.0<m>`) do not contain type variables (e.g. `1.0<'u>`).
    let validateMeasureNoTypars (measure: Measure<SyntaxToken>) (reader: Reader<_, _, _>) =
        let typars = collectTypars measure

        if typars.Count > 0 then
            let mutable state: ParseState = reader.State

            for t in typars do
                let code = DiagnosticCode.TyparInConstant

                let tokStart, tokEnd =
                    match t with
                    | Typar.Anon x -> x, None
                    | Typar.Named(x, x1) -> x, Some x1
                    | Typar.Static(x, x1) -> x, Some x1

                state <- ParseState.addDiagnostic code tokStart tokEnd None state

            reader.State <- state

        preturn () reader

    let parse: FSParser<Constant<_>> =
        parser {
            let! literal = pLiteral

            if literal.Token.IsNumeric then
                let! measure = opt pMeasure

                match measure with
                | ValueSome(lAngle, m, rAngle) ->
                    do! validateMeasureNoTypars m
                    return Constant.MeasuredLiteral(literal, lAngle, m, rAngle)
                | ValueNone -> return Constant.Literal literal
            else
                return Constant.Literal literal
        }
