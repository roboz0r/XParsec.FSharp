namespace XParsec.FSharp.Parser

open System.Collections.Immutable
open XParsec.FSharp

[<RequireQualifiedAccess>]
module ConstructorBody =

    let private initOf (e: Expr<'T>) : ConstructorInit<'T> =
        match e with
        | Expr.Record(lBrace = ParenKind.Brace lBrace; inherits = inherits; fieldInitializers = inits; rBrace = rBrace) ->
            let decl =
                match inherits with
                | ValueSome(RecordInherit(decl = decl)) -> ValueSome decl
                | ValueNone -> ValueNone

            ConstructorInit.Fields(lBrace, decl, inits, rBrace)
        | Expr.New(newToken, typ, args) -> ConstructorInit.Delegated(newToken, typ, args)
        | _ -> ConstructorInit.Expression e

    /// The constructor body written as `e`.
    let rec ofExpr (e: Expr<'T>) : ConstructorBody<'T> =
        match e with
        | Expr.Sequential(exprs = exprs) when exprs.Length > 0 ->
            let mutable body = ofExpr exprs[exprs.Length - 1]

            for i = exprs.Length - 2 downto 0 do
                body <- ConstructorBody.Statement(exprs[i], body)

            body
        | Expr.SequentialThen(inner, thenToken, postlude) -> ConstructorBody.Postlude(ofExpr inner, thenToken, postlude)
        | Expr.LetOrUse(
            keyword = LetOrUseKeyword.Let letToken; isRec = isRec; bindings = bindings; body = ValueSome body) ->
            ConstructorBody.Let(letToken, isRec, bindings, ofExpr body)
        | Expr.IfThenElse(ifToken, condition, _, thenExpr, elifs, ValueSome(ElseBranch(expr = elseExpr))) ->
            let mutable elseBranch = ofExpr elseExpr

            for i = elifs.Length - 1 downto 0 do
                elseBranch <-
                    match elifs[i] with
                    | ElifBranch.Elif(elifToken = t; condition = c; expr = x)
                    | ElifBranch.ElseIf(ifToken = t; condition = c; expr = x) ->
                        ConstructorBody.Conditional(t, c, ofExpr x, elseBranch)

            ConstructorBody.Conditional(ifToken, condition, ofExpr thenExpr, elseBranch)
        | Expr.Match(matchToken = matchToken; matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            match armsOf rules with
            | ValueSome arms -> ConstructorBody.Match(matchToken, scrutinee, arms)
            | ValueNone -> ConstructorBody.Init(initOf e)
        | _ -> ConstructorBody.Init(initOf e)

    /// The arms of a `match` whose rules all parsed.
    and private armsOf (rules: ImArr<Rule<'T>>) : ImArr<ConstructorArm<'T>> voption =
        let arms = ImmutableArray.CreateBuilder<ConstructorArm<'T>>(rules.Length)

        for rule in rules do
            match rule with
            | Rule.Rule(pat, guard, _, body) ->
                arms.Add
                    {
                        Pattern = pat
                        Guard = guard
                        Body = ofExpr body
                    }
            | Rule.Missing
            | Rule.SkipsTokens _ -> ()

        match arms.Count = rules.Length with
        | true -> ValueSome(arms.MoveToImmutable())
        | false -> ValueNone

    /// The initialisations of `body` in source order.
    let rec inits (body: ConstructorBody<'T>) : ConstructorInit<'T> seq =
        seq {
            match body with
            | ConstructorBody.Statement(rest = rest)
            | ConstructorBody.Postlude(body = rest)
            | ConstructorBody.Let(body = rest) -> yield! inits rest
            | ConstructorBody.Conditional(thenBranch = thenBranch; elseBranch = elseBranch) ->
                yield! inits thenBranch
                yield! inits elseBranch
            | ConstructorBody.Match(arms = arms) ->
                for arm in arms do
                    yield! inits arm.Body
            | ConstructorBody.Init init -> yield init
        }
