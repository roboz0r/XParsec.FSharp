namespace XParsec.FSharp.Parser

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Traversal utilities for expression and pattern AST nodes, including embedded type annotations.
// Type declarations and signatures are traversed separately.

module CstWalk =

    /// Returns all bindings defined by a member (e.g., getters/setters or standard methods/properties).
    /// Returns an empty array for auto-properties and abstract signatures.
    let memberBindings (d: MethodOrPropDefn<'T>) : ImmutableArray<Binding<'T>> =
        match d with
        | MethodOrPropDefn.Method(defn = b)
        | MethodOrPropDefn.Property(defn = b) -> ImmutableArray.Create b
        | MethodOrPropDefn.PropertyWithGetSet(defns = bs) -> bs
        | MethodOrPropDefn.AutoProperty _
        | MethodOrPropDefn.AbstractSignature _ -> ImmutableArray.Empty

    /// The self identifier of an instance member (`s` in `member s.Add ...`), including `_`.
    let selfIdent (d: MethodOrPropDefn<'T>) : 'T voption =
        match d with
        | MethodOrPropDefn.Method(ident = ValueSome(struct (selfId, _)))
        | MethodOrPropDefn.Property(ident = ValueSome(struct (selfId, _)))
        | MethodOrPropDefn.PropertyWithGetSet(identPrefix = ValueSome(struct (selfId, _))) -> ValueSome selfId
        | _ -> ValueNone

    type ExprWalker<'env> =
        {
            /// Invoked on an expression before traversing its children (if Traverse returned true).
            Visit: 'env -> Expr<SyntaxToken> -> unit
            /// Invoked on an expression after traversing its children (if Traverse returned true).
            Leave: 'env -> Expr<SyntaxToken> -> unit
            /// Controls whether this node and its sub-tree should be traversed.
            Traverse: 'env -> Expr<SyntaxToken> -> bool
            /// Updates the environment for the applied identifier/long identifier in a type application.
            EnterTypeAppApplied: 'env -> ImmutableArray<Type<SyntaxToken>> -> 'env
            /// Updates the environment for a lambda body given its argument patterns.
            EnterFun: 'env -> ImmutableArray<Pat<SyntaxToken>> -> 'env
            /// Updates the environment for a binding RHS, or returns ValueNone to skip it.
            EnterBindingRhs: 'env -> Binding<SyntaxToken> -> 'env voption
            /// Updates the environment for a `let` group given the group's bindings. The result is the
            /// environment of the group's body, and for a `let rec` group also of each binding's RHS.
            EnterLetGroup: 'env -> ImmutableArray<Binding<SyntaxToken>> -> 'env
            /// Updates the environment for an object-expression member body given the member's self
            /// identifier and argument patterns.
            EnterObjectMember: 'env -> SyntaxToken voption -> ImmutableArray<Pat<SyntaxToken>> -> 'env
            /// Updates the environment for an expression evaluated in a closure: the body of a
            /// lambda, a `function` expression, the value of a binding with parameters, the operand
            /// of `lazy`, or the body of an object-expression member. Applied after `EnterFun`,
            /// `EnterBindingRhs` or `EnterObjectMember`.
            EnterClosure: 'env -> Expr<SyntaxToken> -> 'env
            /// Updates the environment for a `for i = ...` loop body using the loop variable token.
            EnterForTo: 'env -> SyntaxToken -> 'env
            /// Updates the environment for a `for pat in ...` loop body.
            EnterForIn: 'env -> Pat<SyntaxToken> -> 'env
            /// Updates the environment for a match case guard and body.
            EnterMatchArm: 'env -> Pat<SyntaxToken> -> 'env
        }

    /// Default walker that visits all nodes without altering the environment.
    let identityExprWalker<'env> : ExprWalker<'env> =
        {
            Visit = fun _ _ -> ()
            Leave = fun _ _ -> ()
            Traverse = fun _ _ -> true
            EnterTypeAppApplied = fun env _ -> env
            EnterFun = fun env _ -> env
            EnterBindingRhs = fun env _ -> ValueSome env
            EnterLetGroup = fun env _ -> env
            EnterObjectMember = fun env _ _ -> env
            EnterClosure = fun env _ -> env
            EnterForTo = fun env _ -> env
            EnterForIn = fun env _ -> env
            EnterMatchArm = fun env _ -> env
        }

    type ExprEdge<'env, 'payload> =
        {
            Environment: 'env
            Payload: 'payload
            Expression: Expr<SyntaxToken>
        }

    type LetGroupWork<'env> =
        {
            Environment: 'env
            Bindings: ImmutableArray<Binding<SyntaxToken>>
            Body: Expr<SyntaxToken> voption
        }

    type ObjectMemberBindingWork<'env> =
        {
            Environment: 'env
            Self: SyntaxToken voption
            Binding: Binding<SyntaxToken>
        }

    type ExprChildWork<'env> =
        | ChildExpr of 'env * Expr<SyntaxToken>
        | TypeAppApplied of ExprEdge<'env, ImmutableArray<Type<SyntaxToken>>>
        | FunBody of ExprEdge<'env, ImmutableArray<Pat<SyntaxToken>>>
        | BindingRhs of 'env * Binding<SyntaxToken>
        | RecursiveLetGroup of LetGroupWork<'env>
        | LetBody of ExprEdge<'env, ImmutableArray<Binding<SyntaxToken>>>
        | ForToBody of ExprEdge<'env, SyntaxToken>
        | ForInBody of ExprEdge<'env, Pat<SyntaxToken>>
        | MatchArm of 'env * Rule<SyntaxToken>
        /// The rules of the `function` expression `Expression`.
        | FunctionRules of ExprEdge<'env, ImmutableArray<Rule<SyntaxToken>>>
        /// An expression evaluated in a closure, other than a lambda body or a binding's value.
        | ClosureBody of 'env * Expr<SyntaxToken>
        | ObjectMember of 'env * MemberDefn<SyntaxToken>
        | ObjectMemberBinding of ObjectMemberBindingWork<'env>

    let private objectMemberWork env (ObjectMembers(memberDefns = defns)) =
        seq {
            for defn in defns do
                yield ObjectMember(env, defn)
        }

    let private objectExprWork env baseCall members interfaceImpls =
        seq {
            let construction =
                match baseCall with
                | BaseCall.AnonBaseCall c
                | BaseCall.NamedBaseCall(construction = c) -> c

            match construction with
            | ObjectConstruction.ObjectConstruction(expr = e) -> yield ChildExpr(env, e)
            | ObjectConstruction.InterfaceConstruction _ -> ()

            yield! objectMemberWork env members

            for InterfaceImpl.InterfaceImpl(objectMembers = objMembers) in interfaceImpls do
                match objMembers with
                | ValueSome members -> yield! objectMemberWork env members
                | ValueNone -> ()
        }

    /// Child work in source order. Environment callbacks are resolved when each work item is consumed.
    let exprChildWork (env: 'env) (e: Expr<SyntaxToken>) : ExprChildWork<'env> seq =
        seq {
            match e with
            | Expr.Const _
            | Expr.EmptyBlock _
            | Expr.LongIdentOrOp _
            | Expr.OptionalArgExpr _
            | Expr.Null _
            | Expr.Wildcard _
            | Expr.Missing
            | Expr.SkipsTokens _
            | Expr.Ident _
            | Expr.SliceAll _
            | Expr.Pat _ -> ()
            | Expr.String(parts = parts) ->
                for part in parts do
                    match part with
                    | StringPart.Expr(expr = holeExpr) -> yield ChildExpr(env, holeExpr)
                    | _ -> ()
            | Expr.TypeApp(
                expr = (Expr.Ident _ | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) as applied); types = types) ->
                yield
                    TypeAppApplied
                        {
                            Environment = env
                            Payload = types
                            Expression = applied
                        }
            | Expr.PrefixApp(prefixOp = op; expr = inner) when op.Token = Token.KWLazy -> yield ClosureBody(env, inner)
            | Expr.EnclosedBlock(expr = inner)
            | Expr.DotLookup(expr = inner)
            | Expr.TypeApp(expr = inner)
            | Expr.PrefixApp(expr = inner)
            | Expr.DynamicLookup(expr = inner)
            | Expr.New(expr = inner)
            | Expr.ControlFlow(expr = inner)
            | Expr.TypeAnnotation(expr = inner)
            | Expr.StaticUpcast(expr = inner)
            | Expr.DynamicTypeTest(expr = inner)
            | Expr.DynamicDowncast(expr = inner)
            | Expr.StaticMemberInvocation(expr = inner)
            | Expr.SliceFrom(expr = inner)
            | Expr.SliceTo(expr = inner) -> yield ChildExpr(env, inner)
            | Expr.App(funcExpr = fn; argExprs = args) ->
                yield ChildExpr(env, fn)

                for arg in args do
                    yield ChildExpr(env, arg)
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) ->
                yield ChildExpr(env, fn)
                yield ChildExpr(env, arg)
            | Expr.InfixApp(leftExpr = left; rightExpr = right)
            | Expr.Assignment(leftExpr = left; rightExpr = right)
            | Expr.SequentialThen(expr = left; thenExpr = right) ->
                yield ChildExpr(env, left)
                yield ChildExpr(env, right)
            | Expr.IndexedLookup(expr = inner; indexExpr = idx) ->
                yield ChildExpr(env, inner)
                yield ChildExpr(env, idx)
            | Expr.Tuple(exprs = exprs)
            | Expr.StructTuple(exprs = exprs)
            | Expr.Sequential(exprs = exprs) ->
                for child in exprs do
                    yield ChildExpr(env, child)
            | Expr.TryFinally(tryExpr = tryE; finallyExpr = finallyE) ->
                yield ChildExpr(env, tryE)
                yield ChildExpr(env, finallyE)
            | Expr.Range(fromExpr = first; toExpr = last)
            | Expr.SliceFromTo(startExpr = first; endExpr = last) ->
                yield ChildExpr(env, first)
                yield ChildExpr(env, last)
            | Expr.SteppedRange(fromExpr = first; stepExpr = step; toExpr = last) ->
                yield ChildExpr(env, first)
                yield ChildExpr(env, step)
                yield ChildExpr(env, last)
            | Expr.LibraryOnlyStaticOptimization(defaultExpr = defaultExpr; clauses = clauses) ->
                yield ChildExpr(env, defaultExpr)

                for clause in clauses do
                    yield ChildExpr(env, clause.OptimizedExpr)
            | Expr.ILIntrinsic(args = args) ->
                for arg in args do
                    yield ChildExpr(env, arg)
            | Expr.Fun(argumentPats = argPats; expr = body) ->
                yield
                    FunBody
                        {
                            Environment = env
                            Payload = argPats
                            Expression = body
                        }
            | Expr.LetOrUse(isRec = isRec; bindings = bindings; body = body) ->
                if isRec.IsSome then
                    yield
                        RecursiveLetGroup
                            {
                                Environment = env
                                Bindings = bindings
                                Body = body
                            }
                else
                    for binding in bindings do
                        yield BindingRhs(env, binding)

                    match body with
                    | ValueSome bodyExpr ->
                        yield
                            LetBody
                                {
                                    Environment = env
                                    Payload = bindings
                                    Expression = bodyExpr
                                }
                    | ValueNone -> ()
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                yield ChildExpr(env, cond)
                yield ChildExpr(env, thenE)

                for elif_ in elifs do
                    let elifCond, elifExpr =
                        match elif_ with
                        | ElifBranch.Elif(condition = c; expr = x)
                        | ElifBranch.ElseIf(condition = c; expr = x) -> c, x

                    yield ChildExpr(env, elifCond)
                    yield ChildExpr(env, elifExpr)

                match elseB with
                | ValueSome(ElseBranch(expr = elseExpr)) -> yield ChildExpr(env, elseExpr)
                | ValueNone -> ()
            | Expr.While(condition = cond; body = body) ->
                yield ChildExpr(env, cond)
                yield ChildExpr(env, body)
            | Expr.ForTo(ident = ident; startExpr = first; endExpr = last; body = body) ->
                yield ChildExpr(env, first)
                yield ChildExpr(env, last)

                yield
                    ForToBody
                        {
                            Environment = env
                            Payload = ident
                            Expression = body
                        }
            | Expr.ForIn(pat = pat; enumerableExpr = source; body = body) ->
                yield ChildExpr(env, source)

                yield
                    ForInBody
                        {
                            Environment = env
                            Payload = pat
                            Expression = body
                        }
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
                yield ChildExpr(env, scrutinee)

                for rule in rules do
                    yield MatchArm(env, rule)
            | Expr.Function(rules = Rules(rules = rules)) ->
                yield
                    FunctionRules
                        {
                            Environment = env
                            Payload = rules
                            Expression = e
                        }
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
                yield ChildExpr(env, body)

                for rule in rules do
                    yield MatchArm(env, rule)
            | Expr.Object(baseCall = baseCall; members = members; interfaceImpls = interfaceImpls) ->
                yield! objectExprWork env baseCall members interfaceImpls
            | Expr.Record(inherits = inherits; fieldInitializers = inits) ->
                match inherits with
                | ValueSome(RecordInherit(decl = ClassInheritsDecl(expr = ValueSome arg))) -> yield ChildExpr(env, arg)
                | _ -> ()

                for FieldInitializer(expr = inner) in inits do
                    yield ChildExpr(env, inner)
            | Expr.RecordClone(expr = source; fieldInitializers = inits) ->
                yield ChildExpr(env, source)

                for FieldInitializer(expr = inner) in inits do
                    yield ChildExpr(env, inner)
        }

    /// Resolves a deferred edge when the traversal reaches it.
    let expandExprChildWork (walker: ExprWalker<'env>) (work: ExprChildWork<'env>) : ExprChildWork<'env> seq =
        seq {
            match work with
            | ChildExpr _ -> yield work
            | TypeAppApplied edge ->
                yield ChildExpr(walker.EnterTypeAppApplied edge.Environment edge.Payload, edge.Expression)
            | FunBody edge ->
                let bodyEnv =
                    walker.EnterClosure (walker.EnterFun edge.Environment edge.Payload) edge.Expression

                yield ChildExpr(bodyEnv, edge.Expression)
            | BindingRhs(env, binding) ->
                match walker.EnterBindingRhs env binding with
                | ValueSome rhsEnv when binding.argumentPats.IsEmpty -> yield ChildExpr(rhsEnv, binding.expr)
                | ValueSome rhsEnv -> yield ChildExpr(walker.EnterClosure rhsEnv binding.expr, binding.expr)
                | ValueNone -> ()
            | ClosureBody(env, body) -> yield ChildExpr(walker.EnterClosure env body, body)
            | RecursiveLetGroup group ->
                let groupEnv = walker.EnterLetGroup group.Environment group.Bindings

                for binding in group.Bindings do
                    yield BindingRhs(groupEnv, binding)

                match group.Body with
                | ValueSome bodyExpr -> yield ChildExpr(groupEnv, bodyExpr)
                | ValueNone -> ()
            | LetBody edge -> yield ChildExpr(walker.EnterLetGroup edge.Environment edge.Payload, edge.Expression)
            | ForToBody edge -> yield ChildExpr(walker.EnterForTo edge.Environment edge.Payload, edge.Expression)
            | ForInBody edge -> yield ChildExpr(walker.EnterForIn edge.Environment edge.Payload, edge.Expression)
            | MatchArm(env, rule) ->
                match rule with
                | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                    let armEnv = walker.EnterMatchArm env pat

                    match guard with
                    | ValueSome(PatternGuard(expr = guardExpr)) -> yield ChildExpr(armEnv, guardExpr)
                    | ValueNone -> ()

                    yield ChildExpr(armEnv, body)
                | _ -> ()
            | FunctionRules edge ->
                let bodyEnv =
                    walker.EnterClosure (walker.EnterFun edge.Environment ImmutableArray.Empty) edge.Expression

                for rule in edge.Payload do
                    yield MatchArm(bodyEnv, rule)
            | ObjectMember(env, defn) ->
                match defn with
                | MemberDefn.Member(defn = MethodOrPropDefn.AutoProperty(expr = expr)) -> yield ChildExpr(env, expr)
                | MemberDefn.Member(defn = memberDefn) ->
                    let self = selfIdent memberDefn

                    for binding in memberBindings memberDefn do
                        yield
                            ObjectMemberBinding
                                {
                                    Environment = env
                                    Self = self
                                    Binding = binding
                                }
                | MemberDefn.Value _ -> ()
                | MemberDefn.AdditionalConstructor _ ->
                    failwith "CstWalk.iterObjectMembers: TODO AdditionalConstructor in object expression"
            | ObjectMemberBinding memberWork ->
                let body = memberWork.Binding.expr

                let memberEnv =
                    walker.EnterObjectMember memberWork.Environment memberWork.Self memberWork.Binding.argumentPats

                yield ChildExpr(walker.EnterClosure memberEnv body, body)
        }

    // Environment modifications returned by Enter* callbacks apply only to the subtree they are passed to.
    let rec iterExpr (walker: ExprWalker<'env>) (env: 'env) (e: Expr<SyntaxToken>) : unit =
        if walker.Traverse env e then
            walker.Visit env e

            for child in exprChildWork env e do
                iterExprChildWork walker child

            walker.Leave env e

    and private iterExprChildWork (walker: ExprWalker<'env>) (work: ExprChildWork<'env>) : unit =
        match work with
        | ChildExpr(env, expr) -> iterExpr walker env expr
        | _ ->
            for child in expandExprChildWork walker work do
                iterExprChildWork walker child

    /// Traverses a binding's RHS in the environment returned by `EnterBindingRhs`.
    /// Skips traversal if `EnterBindingRhs` returns `ValueNone`.
    let iterBindingRhs (walker: ExprWalker<'env>) (env: 'env) (b: Binding<SyntaxToken>) : unit =
        iterExprChildWork walker (BindingRhs(env, b))

    /// Traverses type annotations and member signatures embedded directly on this expression node.
    /// Does not recurse into child expressions or pattern annotations.
    let iterExprEmbeddedTypes (it: CstTypeWalk.TypeIter) (e: Expr<SyntaxToken>) : unit =
        let onType = CstTypeWalk.iterType it
        let onMemberSig = CstTypeWalk.iterTypeMemberSig it

        let memberDefnSigs (defns: ImmutableArray<MemberDefn<SyntaxToken>>) : unit =
            for d in defns do
                CstTypeWalk.iterMemberDefnSigTypes it ignore d

        match e with
        // Unit-of-measure annotations on numeric literals (e.g. `1.0<m>`).
        | Expr.Const(Constant.MeasuredLiteral(measure = m)) -> CstTypeWalk.iterMeasure it m

        // Expressions that do not directly declare types.
        | Expr.Const _
        | Expr.EmptyBlock _
        | Expr.LongIdentOrOp _
        | Expr.OptionalArgExpr _
        | Expr.Null _
        | Expr.Wildcard _
        | Expr.Missing
        | Expr.SkipsTokens _
        | Expr.Ident _
        | Expr.SliceAll _
        | Expr.String _
        | Expr.EnclosedBlock _
        | Expr.DotLookup _
        | Expr.PrefixApp _
        | Expr.DynamicLookup _
        | Expr.ControlFlow _
        | Expr.SliceFrom _
        | Expr.SliceTo _
        | Expr.App _
        | Expr.HighPrecedenceApp _
        | Expr.InfixApp _
        | Expr.Assignment _
        | Expr.IndexedLookup _
        | Expr.Tuple _
        | Expr.StructTuple _
        | Expr.Sequential _
        | Expr.SequentialThen _
        | Expr.TryFinally _
        | Expr.Range _
        | Expr.SliceFromTo _
        | Expr.SteppedRange _
        | Expr.Fun _
        | Expr.IfThenElse _
        | Expr.While _
        | Expr.ForTo _
        | Expr.ForIn _
        | Expr.Match _
        | Expr.Function _
        | Expr.TryWith _
        | Expr.Record(inherits = ValueNone)
        | Expr.RecordClone _
        | Expr.Pat _ -> ()

        | Expr.Record(inherits = ValueSome(RecordInherit(decl = ClassInheritsDecl(typ = t)))) -> onType t

        | Expr.New(typ = t)
        | Expr.TypeAnnotation(typ = t)
        | Expr.StaticUpcast(typ = t)
        | Expr.DynamicTypeTest(typ = t)
        | Expr.DynamicDowncast(typ = t) -> onType t

        // Return type annotation on inline IL (`(# "..." : T #)`).
        | Expr.ILIntrinsic(returnType = rt) ->
            match rt with
            | ValueSome(ReturnType(typ = t)) -> onType t
            | ValueNone -> ()

        | Expr.TypeApp(types = types) ->
            for t in types do
                onType t

        | Expr.LetOrUse(bindings = bindings) ->
            for b in bindings do
                CstTypeWalk.iterBindingSigTypes onType onMemberSig b

        // SRTP member constraint calls (`((^T) : (static member ...))` args).
        | Expr.StaticMemberInvocation(membersign = ms) -> onMemberSig ms

        // Type constraints in static optimization clauses (e.g. `when ^T : int`).
        | Expr.LibraryOnlyStaticOptimization(clauses = clauses) ->
            for clause in clauses do
                for c in clause.Constraints do
                    match c with
                    | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(rhsType = rhs) -> onType rhs
                    | StaticOptimizationConstraint.WhenTyparIsStruct _ -> ()

        | Expr.Object(baseCall = baseCall; members = members; interfaceImpls = impls) ->
            let ctorTy =
                match baseCall with
                | BaseCall.AnonBaseCall c
                | BaseCall.NamedBaseCall(construction = c) ->
                    match c with
                    | ObjectConstruction.ObjectConstruction(typ = t)
                    | ObjectConstruction.InterfaceConstruction(typ = t) -> t

            onType ctorTy

            let (ObjectMembers(memberDefns = memberDefns)) = members
            memberDefnSigs memberDefns

            for InterfaceImpl.InterfaceImpl(typ = t; objectMembers = objMembers) in impls do
                onType t

                match objMembers with
                | ValueSome(ObjectMembers(memberDefns = ds)) -> memberDefnSigs ds
                | ValueNone -> ()

    /// Visitor for pattern AST nodes.
    /// `VisitPat` is called before child patterns are visited; returning `false` skips child traversal.
    [<NoEquality; NoComparison>]
    type PatIter =
        {
            VisitPat: PatIter -> Pat<SyntaxToken> -> bool
        }

    let rec iterPat (it: PatIter) (p: Pat<SyntaxToken>) : unit =
        if it.VisitPat it p then
            let walk = iterPat it

            match p with
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner)
            | Pat.As(pat = inner)
            | Pat.Optional(pat = inner)
            | Pat.TypeTestAs(pat = inner) -> walk inner
            | Pat.Named(argumentPats = args)
            | Pat.OpNamed(argumentPats = args) ->
                for sub in args do
                    walk sub
            | Pat.NamedFieldPats(args = args) ->
                for a in args do
                    match a with
                    | UnionArgPat.Named(pat = sub)
                    | UnionArgPat.Positional(pat = sub) -> walk sub
            | Pat.Tuple(patterns = pats)
            | Pat.StructTuple(patterns = pats)
            | Pat.Elems(pats = pats) ->
                for sub in pats do
                    walk sub
            | Pat.Record(fieldPats = fieldPats) ->
                for FieldPat(pat = sub) in fieldPats do
                    walk sub
            | Pat.Cons(head = a; tail = b)
            | Pat.Or(left = a; right = b)
            | Pat.And(left = a; right = b) ->
                walk a
                walk b
            // Terminal pattern nodes without nested patterns.
            | Pat.NamedSimple _
            | Pat.TypeTest _
            | Pat.Const _
            | Pat.EmptyBlock _
            | Pat.Wildcard _
            | Pat.Null _
            | Pat.Op _
            | Pat.String _
            | Pat.Expr _
            | Pat.Missing
            | Pat.SkipsTokens _ -> ()
