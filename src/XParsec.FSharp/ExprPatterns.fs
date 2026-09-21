namespace XParsec.FSharp.Parser

open XParsec.FSharp.Lexer

/// Active patterns to decompose generic `Expr.PrefixApp` nodes into
/// specific constructs (e.g., `lazy`, `assert`, casts, and quotation splices).
module ExprPatterns =

    let inline private kwPrefix (kw: Token) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.PrefixApp(t, inner) when t.Token = kw -> ValueSome(struct (t, inner))
        | _ -> ValueNone

    let (|LazyExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWLazy e
    let (|AssertExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWAssert e
    let (|FixedExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWFixed e
    let (|UpcastExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWUpcast e
    let (|DowncastExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWDowncast e

    /// Matches typed quotation splices (`%expr`).
    let (|ExpressionSplice|_|) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.PrefixApp(t, inner) when t.Token = Token.OpSplice -> ValueSome(struct (t, inner))
        | _ -> ValueNone

    /// Matches untyped quotation splices (`%%expr`).
    let (|WeaklyTypedExpressionSplice|_|) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.PrefixApp(t, inner) when t.Token = Token.OpSpliceUntyped -> ValueSome(struct (t, inner))
        | _ -> ValueNone
