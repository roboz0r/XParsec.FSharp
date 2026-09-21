namespace XParsec.FSharp.Parser

open XParsec
open XParsec.FSharp.Lexer

[<AutoOpen>]
module internal ParserRefs =
    // Mutually recursive parsers broken across multiple files using forward references.
    type FSRefParser<'T> = RefParser<'T, PositionedToken, ParseState, FSReadable>

    let refObjectConstruction = FSRefParser<ObjectConstruction<SyntaxToken>>()
    let refType = FSRefParser<Type<SyntaxToken>>()

    /// Curried member signature, shared by abstract members and SRTP trait constraints.
    let refCurriedSig = FSRefParser<CurriedSig<SyntaxToken>>()

    let refMeasure = FSRefParser<Measure<SyntaxToken>>()
    let refExpr = FSRefParser<Expr<SyntaxToken>>()
    let refExprSeqBlock = FSRefParser<Expr<SyntaxToken>>()

    /// A sequence block with an optional type annotation (e.g. lambda bodies, match clauses, `if/then/else` branches).
    let refTypedSeqExprBlock = FSRefParser<Expr<SyntaxToken>>()

    /// Guard expression in a pattern (`when <expr> ->`), stopping before `->`.
    let refExprGuard = FSRefParser<Expr<SyntaxToken>>()

    /// Expression that stops before sequential separators (semicolons/newlines).
    /// Used in `while <expr> do` so a trailing `do` is not swallowed as a `do <expr>` statement.
    let refExprNoSeq = FSRefParser<Expr<SyntaxToken>>()

    /// Field/element expression inside records and collection initializers, stopping before `;`.
    let refExprInRecords = FSRefParser<Expr<SyntaxToken>>()

    /// Single atomic expression (literal, identifier, parenthesized expr).
    let refExprAtomic = FSRefParser<Expr<SyntaxToken>>()

    /// IL intrinsic argument (allows `.`, `[]`, and type applications).
    /// Stops at whitespace so arguments in `(# "op" a b c : ty #)` are parsed separately.
    let refExprILArg = FSRefParser<Expr<SyntaxToken>>()
