namespace XParsec.FSharp.Parser

module CstTokens =

    let firstTokenOfLongIdent (li: LongIdent<SyntaxToken>) : SyntaxToken = li.Idents.[0]

    let firstTokenOfIdentOrOp (op: IdentOrOp<SyntaxToken>) : SyntaxToken =
        match op with
        | IdentOrOp.Ident t -> t
        | IdentOrOp.ParenOp(lParen, _, _) -> lParen

    let firstTokenOfLongIdentOrOp (lio: LongIdentOrOp<SyntaxToken>) : SyntaxToken =
        match lio with
        | LongIdentOrOp.LongIdent li -> firstTokenOfLongIdent li
        | LongIdentOrOp.Op op -> firstTokenOfIdentOrOp op
        | LongIdentOrOp.QualifiedOp(li, _, _) -> firstTokenOfLongIdent li

    let rec firstTokenOfMeasure (m: Measure<SyntaxToken>) : SyntaxToken =
        match m with
        | Measure.Named li -> firstTokenOfLongIdent li
        | Measure.One t
        | Measure.Anonymous t -> t
        | Measure.Typar(Typar.Anon t)
        | Measure.Typar(Typar.Named(quote = t))
        | Measure.Typar(Typar.Static(caret = t)) -> t
        | Measure.Juxtaposition(elems, _) -> firstTokenOfMeasure elems.[0]
        | Measure.Power(inner, _, _, _)
        | Measure.Product(inner, _, _)
        | Measure.Quotient(inner, _, _) -> firstTokenOfMeasure inner
        | Measure.Reciprocal(t, _)
        | Measure.Paren(t, _, _) -> t

    let typarToken (t: Typar<SyntaxToken>) : SyntaxToken voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome id
        | Typar.Anon _ -> ValueNone

    let firstTokenOfConstant (c: Constant<SyntaxToken>) : SyntaxToken =
        match c with
        | Constant.Literal t -> t
        | Constant.MeasuredLiteral(value = t) -> t

    let firstTokenOfParenKind (pk: ParenKind<SyntaxToken>) : SyntaxToken =
        match pk with
        | ParenKind.Paren t
        | ParenKind.BeginEnd t
        | ParenKind.List t
        | ParenKind.Array t
        | ParenKind.Brace t
        | ParenKind.BraceBar t
        | ParenKind.Quoted t
        | ParenKind.DoubleQuoted t -> t

    let firstTokenOfLetOrUseKeyword (kw: LetOrUseKeyword<SyntaxToken>) : SyntaxToken =
        match kw with
        | LetOrUseKeyword.Let t
        | LetOrUseKeyword.LetBang t
        | LetOrUseKeyword.Use t
        | LetOrUseKeyword.UseBang t -> t

    let firstTokenOfControlFlowKeyword (kw: ControlFlowKeyword<SyntaxToken>) : SyntaxToken =
        match kw with
        | ControlFlowKeyword.Yield t
        | ControlFlowKeyword.YieldBang t
        | ControlFlowKeyword.Return t
        | ControlFlowKeyword.ReturnBang t
        | ControlFlowKeyword.Do t
        | ControlFlowKeyword.DoBang t -> t

    /// Gets the opening delimiter of a string literal.
    let firstTokenOfStringKind (kind: StringKind<SyntaxToken>) : SyntaxToken =
        match kind with
        | StringKind.String t
        | StringKind.VerbatimString t
        | StringKind.String3 t
        | StringKind.InterpolatedString t
        | StringKind.VerbatimInterpolatedString t
        | StringKind.Interpolated3String t -> t

    let rec firstTokenOfExpr (e: Expr<SyntaxToken>) : SyntaxToken =
        match e with
        | Expr.Const c -> firstTokenOfConstant c
        | Expr.Ident t -> t
        | Expr.LongIdentOrOp lio -> firstTokenOfLongIdentOrOp lio
        | Expr.App(funcExpr, _) -> firstTokenOfExpr funcExpr
        | Expr.HighPrecedenceApp(funcExpr = funcExpr) -> firstTokenOfExpr funcExpr
        | Expr.InfixApp(_, op, _) -> op
        | Expr.PrefixApp(op, _) -> op
        | Expr.Range(fromExpr = fromE) -> firstTokenOfExpr fromE
        | Expr.SteppedRange(fromExpr = fromE) -> firstTokenOfExpr fromE
        | Expr.Null(nullToken = t) -> t
        | Expr.LetOrUse(keyword = kw) -> firstTokenOfLetOrUseKeyword kw
        | Expr.Fun(funToken = t) -> t
        | Expr.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Expr.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Expr.IfThenElse(ifToken = t) -> t
        | Expr.Tuple(exprs = exprs) ->
            match exprs.Length with
            | 0 -> failwithf "CstTokens.firstTokenOfExpr: tuple expression retains no token: %A" e
            | _ -> firstTokenOfExpr exprs.[0]
        | Expr.Sequential(exprs = exprs) ->
            match exprs.Length with
            | 0 -> failwithf "CstTokens.firstTokenOfExpr: sequential expression retains no token: %A" e
            | _ -> firstTokenOfExpr exprs.[0]
        | Expr.SequentialThen(expr = inner) -> firstTokenOfExpr inner
        | Expr.StructTuple(structToken = t) -> t
        | Expr.TypeAnnotation(expr = inner) -> firstTokenOfExpr inner
        | Expr.StaticUpcast(colonGreaterThan = t) -> t
        | Expr.DynamicTypeTest(colonQuestionMark = t) -> t
        | Expr.DynamicDowncast(colonQuestionMarkGreaterThan = t) -> t
        | Expr.While(whileToken = t) -> t
        | Expr.ForTo(forToken = t) -> t
        | Expr.ForIn(forToken = t) -> t
        | Expr.Match(matchToken = t) -> t
        | Expr.Function(functionToken = t) -> t
        | Expr.TryWith(tryToken = t) -> t
        | Expr.TryFinally(tryToken = t) -> t
        | Expr.Assignment(arrow = t) -> t
        | Expr.String(kind = kind) -> firstTokenOfStringKind kind
        | Expr.DotLookup(expr = inner) -> firstTokenOfExpr inner
        | Expr.DynamicLookup(questionMark = t) -> t
        | Expr.TypeApp(expr = inner) -> firstTokenOfExpr inner
        | Expr.Record(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.RecordClone(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.New(newToken = t) -> t
        | Expr.Object(lBrace = t) -> t
        | Expr.ILIntrinsic(lHashParen = t) -> t
        | Expr.LibraryOnlyStaticOptimization(defaultExpr = defaultExpr; clauses = clauses) ->
            match clauses.Length with
            | 0 -> firstTokenOfExpr defaultExpr
            | _ -> clauses.[0].WhenToken
        | Expr.IndexedLookup(lBracket = t) -> t
        | Expr.StaticMemberInvocation(lParen = t) -> t
        | Expr.OptionalArgExpr(questionMark = t) -> t
        | Expr.ControlFlow(keyword = kw) -> firstTokenOfControlFlowKeyword kw
        | Expr.Wildcard(underscore = t) -> t
        | Expr.SliceFrom(expr = inner) -> firstTokenOfExpr inner
        | Expr.SliceTo(dotdot = t) -> t
        | Expr.SliceFromTo(startExpr = inner) -> firstTokenOfExpr inner
        | Expr.SliceAll(star = t) -> t
        | Expr.SkipsTokens tokens ->
            match tokens.Length with
            | 0 -> failwithf "CstTokens.firstTokenOfExpr: skipped-token expression retains no token: %A" e
            | _ -> tokens.[0]
        | Expr.Missing -> failwithf "CstTokens.firstTokenOfExpr: missing expression retains no token"

    and firstTokenOfPat (p: Pat<SyntaxToken>) : SyntaxToken =
        match p with
        | Pat.Const c -> firstTokenOfConstant c
        | Pat.NamedSimple t -> t
        | Pat.Named(longIdent = li) -> firstTokenOfLongIdent li
        | Pat.Wildcard t -> t
        | Pat.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.Tuple(patterns = pats) ->
            match pats.Length with
            | 0 -> failwithf "CstTokens.firstTokenOfPat: tuple pattern retains no token: %A" p
            | _ -> firstTokenOfPat pats.[0]
        | Pat.StructTuple(structToken = t) -> t
        | Pat.Elems(pats = pats) ->
            match pats.Length with
            | 0 -> failwithf "CstTokens.firstTokenOfPat: element-list pattern retains no token: %A" p
            | _ -> firstTokenOfPat pats.[0]
        | Pat.Typed(pat = inner) -> firstTokenOfPat inner
        | Pat.Attributed(pat = inner) -> firstTokenOfPat inner
        | Pat.As(pat = inner) -> firstTokenOfPat inner
        | Pat.Or(left = inner) -> firstTokenOfPat inner
        | Pat.And(left = inner) -> firstTokenOfPat inner
        | Pat.Record(lBrace = t) -> t
        | Pat.Op io -> firstTokenOfIdentOrOp io
        | Pat.OpNamed(ident = io) -> firstTokenOfIdentOrOp io
        | Pat.NamedFieldPats(longIdent = li) -> firstTokenOfLongIdent li
        | Pat.Cons(consToken = t) -> t
        | Pat.TypeTestAs(colonQuestion = t) -> t
        | Pat.TypeTest(colonQuestion = t) -> t
        | Pat.Null t -> t
        | Pat.Optional(questionMark = t) -> t
        | Pat.String(kind = kind) -> firstTokenOfStringKind kind
        | Pat.Expr(expr = inner) -> firstTokenOfExpr inner
        | Pat.SkipsTokens tokens ->
            match tokens.Length with
            | 0 -> failwithf "CstTokens.firstTokenOfPat: skipped-token pattern retains no token: %A" p
            | _ -> tokens.[0]
        | Pat.Missing -> failwithf "CstTokens.firstTokenOfPat: missing pattern retains no token"

    /// Gets the diagnostic token for an expression; member accesses use the accessed member's token.
    let diagTokenOfExpr (e: Expr<SyntaxToken>) : SyntaxToken =
        match e with
        | Expr.DotLookup(longIdentOrOp = lio)
        | Expr.App(funcExpr = Expr.DotLookup(longIdentOrOp = lio))
        | Expr.HighPrecedenceApp(funcExpr = Expr.DotLookup(longIdentOrOp = lio)) -> firstTokenOfLongIdentOrOp lio
        | _ -> firstTokenOfExpr e

    let rec tryFirstTokenOfType (t: Type<SyntaxToken>) : SyntaxToken voption =
        match t with
        | Type.ParenType(lParen = l) -> ValueSome l
        | Type.FunctionType(fromType = f) -> tryFirstTokenOfType f
        | Type.TupleType(types = ts) ->
            if ts.Length > 0 then
                tryFirstTokenOfType ts.[0]
            else
                ValueNone
        | Type.StructTupleType(structToken = s) -> ValueSome s
        | Type.VarType tp ->
            match tp with
            | Typar.Anon u -> ValueSome u
            | Typar.Named(quote = q) -> ValueSome q
            | Typar.Static(caret = c) -> ValueSome c
        | Type.NamedType li
        | Type.GenericType(longIdent = li) ->
            (if li.Idents.Length > 0 then
                 ValueSome li.Idents.[0]
             else
                 ValueNone)
        | Type.SuffixedType(baseType = b)
        | Type.DottedType(baseType = b)
        | Type.ArrayType(baseType = b) -> tryFirstTokenOfType b
        | Type.WhenConstrainedType(typ = inner) -> tryFirstTokenOfType inner
        | Type.AnonymousSubtype(hash = h) -> ValueSome h
        | Type.Null t -> ValueSome t
        | Type.UnionType(left = l) -> tryFirstTokenOfType l
        | Type.ILIntrinsic(lHashParen = l) -> ValueSome l
        | Type.AnonRecordType(lBraceBar = l) -> ValueSome l
        | _ -> ValueNone
