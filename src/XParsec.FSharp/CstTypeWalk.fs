namespace XParsec.FSharp.Parser

open XParsec.FSharp.Parser

/// Traverses type definitions and type references within Concrete Syntax Tree (CST) nodes.

module CstTypeWalk =

    type ITypeVisitor =
        /// Visits a type before its children; returning false skips child traversal.
        abstract VisitType: Type<SyntaxToken> -> bool
        /// Visits measure identifiers outside standard type nodes.
        abstract VisitMeasureName: LongIdent<SyntaxToken> -> unit

    module TypeVisitor =
        let ofTypes (visit: Type<SyntaxToken> -> bool) : ITypeVisitor =
            { new ITypeVisitor with
                member _.VisitType ty = visit ty
                member _.VisitMeasureName _ = ()
            }

    /// Traverses all named identifiers within a unit of measure expression.
    let rec iterMeasure (it: ITypeVisitor) (m: Measure<SyntaxToken>) : unit =
        match m with
        | Measure.Named li -> it.VisitMeasureName li
        | Measure.Power(inner, _, _, _)
        | Measure.Reciprocal(_, inner)
        | Measure.Paren(_, inner, _) -> iterMeasure it inner
        | Measure.Product(l, _, r)
        | Measure.Quotient(l, _, r) ->
            iterMeasure it l
            iterMeasure it r
        | Measure.Juxtaposition(elems, _) ->
            for e in elems do
                iterMeasure it e
        | Measure.One _
        | Measure.Anonymous _
        | Measure.Typar _ -> ()

    let rec iterType (it: ITypeVisitor) (ty: Type<SyntaxToken>) : unit =
        if it.VisitType ty then
            let walk = iterType it

            match ty with
            | Type.ParenType(typ = inner)
            | Type.SuffixedType(baseType = inner)
            | Type.DottedType(baseType = inner)
            | Type.ArrayType(baseType = inner)
            | Type.AnonymousSubtype(typ = inner)
            | Type.SubtypeConstraint(typ = inner) -> walk inner
            | Type.FunctionType(fromType = f; toType = t) ->
                walk f
                walk t
            | Type.TupleType(types = ts)
            | Type.StructTupleType(types = ts) ->
                for t in ts do
                    walk t
            | Type.GenericType(typeArgs = args) ->
                for a in args do
                    match a with
                    | TypeArg.Type at -> walk at
                    | TypeArg.Measure m -> iterMeasure it m
            | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
                walk inner
                iterTypeConstraints it cs
            | Type.UnionType(left = l; right = r) ->
                walk l
                walk r
            | Type.AnonRecordType(fields = fs) ->
                for AnonRecordField(typ = t) in fs do
                    walk t
            | Type.MeasureType m -> iterMeasure it m
            // Leaf nodes containing no child Type nodes.
            | Type.VarType _
            | Type.NamedType _
            | Type.Null _
            | Type.ILIntrinsic _
            | Type.Missing
            | Type.SkipsTokens _ -> ()

    and iterTypeConstraints (it: ITypeVisitor) (cs: TyparConstraints<SyntaxToken>) : unit =
        for c in cs.Constraints do
            iterTypeConstraint it c

    and iterTypeConstraint (it: ITypeVisitor) (c: Constraint<SyntaxToken>) : unit =
        iterConstraintTypes (iterType it) (iterTypeMemberSig it) c

    /// Traverses types and member signatures referenced within a type constraint (`when` clause).
    and iterConstraintTypes
        (onType: Type<SyntaxToken> -> unit)
        (onMemberSig: MemberSig<SyntaxToken> -> unit)
        (c: Constraint<SyntaxToken>)
        : unit =
        match c with
        | Constraint.Coercion(typ = t)
        | Constraint.Enum(typ = t)
        | Constraint.Default(typ = t) -> onType t
        | Constraint.Delegate(type1 = t1; type2 = t2) ->
            onType t1
            onType t2
        | Constraint.MemberTrait(membersign = ms) -> onMemberSig ms
        // Constraints that do not reference any Type nodes.
        | Constraint.Nullness _
        | Constraint.DefaultConstructor _
        | Constraint.Struct _
        | Constraint.ReferenceType _
        | Constraint.NotNull _
        | Constraint.Unmanaged _
        | Constraint.Equality _
        | Constraint.Comparison _ -> ()

    and iterTypeMemberSig (it: ITypeVisitor) (ms: MemberSig<SyntaxToken>) : unit =
        match ms with
        | MemberSig.MethodOrPropSig(typarDefns = tds; sign = cs) ->
            match tds with
            | ValueSome(TyparDefns(constraints = ValueSome tcs)) -> iterTypeConstraints it tcs
            | _ -> ()

            iterTypeCurriedSig it cs
        | MemberSig.PropSig(sign = cs) -> iterTypeCurriedSig it cs

    and iterTypeCurriedSig (it: ITypeVisitor) (cs: CurriedSig<SyntaxToken>) : unit =
        let (CurriedSig(args = argGroups; returnType = ret)) = cs

        for struct (argsSpec, _) in argGroups do
            let (ArgsSpec.ArgsSpec(args = args)) = argsSpec

            for (ArgSpec(typ = t)) in args do
                iterType it t

        iterType it ret

    /// Performs a pre-order traversal of `ty`, returning the first value where `f` returns `ValueSome`.
    let tryPickType (f: Type<SyntaxToken> -> 'a voption) (ty: Type<SyntaxToken>) : 'a voption =
        let mutable found = ValueNone

        let visitType (t: Type<SyntaxToken>) =
            if found.IsNone then
                found <- f t

            found.IsNone

        iterType (TypeVisitor.ofTypes visitType) ty

        found

    /// Returns true if any `Type` node inside `ty` satisfies `pred`.
    let existsType (pred: Type<SyntaxToken> -> bool) (ty: Type<SyntaxToken>) : bool =
        tryPickType (fun t -> if pred t then ValueSome() else ValueNone) ty
        |> ValueOption.isSome

    /// Traverses the types in an uncurried signature, such as a delegate signature
    /// or a GADT-style union case signature (`Case : arg * arg -> ret`).
    let iterTypeUncurriedSig (it: ITypeVisitor) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            iterType it t

        iterType it ret

    /// Traverses all types declared in a discriminated union case, supporting standard fields
    /// and GADT syntax.
    let iterTypeUnionCase (it: ITypeVisitor) (UnionTypeCase(data = data)) : unit =
        match data with
        | UnionTypeCaseData.Nullary _ -> ()
        | UnionTypeCaseData.Nary(fields = fs) ->
            for f in fs do
                match f with
                | UnionTypeField.Unnamed(typ = t)
                | UnionTypeField.Named(typ = t) -> iterType it t
        | UnionTypeCaseData.GadtNary(sign = s) -> iterTypeUncurriedSig it s
        | UnionTypeCaseData.GadtNullary(typ = t) -> iterType it t

    /// Traverses the type constraints defined directly on a type declaration's header.
    let iterTypeNameConstraints (it: ITypeVisitor) (tn: TypeName<SyntaxToken>) : unit =
        let (TypeName(typarDefns = tds; postfixConstraints = post)) = tn

        match tds with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) -> iterTypeConstraints it cs
        | _ -> ()

        match post with
        | ValueSome cs -> iterTypeConstraints it cs
        | ValueNone -> ()

    /// Traverses type annotations on a let-binding's return type and generic constraints.
    /// (Note: parameter type annotations are embedded in pattern AST nodes and visited separately.)
    let iterBindingSigTypes
        (onType: Type<SyntaxToken> -> unit)
        (onMemberSig: MemberSig<SyntaxToken> -> unit)
        (b: Binding<SyntaxToken>)
        : unit =
        match b.typarDefns with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) ->
            for c in cs.Constraints do
                iterConstraintTypes onType onMemberSig c
        | _ -> ()

        match b.returnType with
        | ValueSome(ReturnType(typ = t)) -> onType t
        | ValueNone -> ()

    /// Traverses type annotations in a member definition's signature (return types,
    /// auto-property types, abstract slots, and `val` fields). Parameter patterns are delegated to `onPat`.
    let iterMemberDefnSigTypes
        (it: ITypeVisitor)
        (onPat: Pat<SyntaxToken> -> unit)
        (md: MemberDefn<SyntaxToken>)
        : unit =
        let onType = iterType it
        let onMemberSig = iterTypeMemberSig it

        let bindingSig (b: Binding<SyntaxToken>) =
            for ap in b.argumentPats do
                onPat ap

            iterBindingSigTypes onType onMemberSig b

        match md with
        | MemberDefn.Member(defn = d) ->
            match d with
            | MethodOrPropDefn.Method(defn = b)
            | MethodOrPropDefn.Property(defn = b) -> bindingSig b
            | MethodOrPropDefn.PropertyWithGetSet(defns = bs) ->
                for b in bs do
                    bindingSig b
            | MethodOrPropDefn.AutoProperty(returnType = ValueSome(ReturnType(typ = t))) -> onType t
            | MethodOrPropDefn.AutoProperty _ -> ()
            | MethodOrPropDefn.AbstractSignature sign -> onMemberSig sign
        | MemberDefn.Value(typ = t) -> onType t
        | MemberDefn.AdditionalConstructor(pat = p) -> onPat p

    /// Traverses the types declared within a type definition's structural surface
    /// (base types, interfaces, fields, and member signatures), excluding method and property implementation bodies.
    let iterTypeDefnTypes
        (it: ITypeVisitor)
        (onPat: Pat<SyntaxToken> -> unit)
        (onInherit: Type<SyntaxToken> -> unit)
        (td: TypeDefn<SyntaxToken>)
        : unit =
        let ty (t: Type<SyntaxToken>) = iterType it t

        let memberDefn (md: MemberDefn<SyntaxToken>) = iterMemberDefnSigTypes it onPat md

        let element (el: TypeDefnElement<SyntaxToken>) =
            match el with
            | TypeDefnElement.Member md -> memberDefn md
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(typ = t; objectMembers = oms)) ->
                ty t

                match oms with
                | ValueSome(ObjectMembers(memberDefns = mds)) ->
                    for md in mds do
                        memberDefn md
                | ValueNone -> ()
            | TypeDefnElement.InterfaceSpec(InterfaceSpec.InterfaceSpec(typ = t)) -> ty t
            | TypeDefnElement.Inherit(ClassInheritsDecl(typ = t)) -> onInherit t

        // Class preamble `let` bindings are implementation details; only their explicit type
        // annotations and constraints contribute to the type's surface signature.
        let preamble (d: ClassFunctionOrValueDefn<SyntaxToken>) =
            match d with
            | ClassFunctionOrValueDefn.LetBindings(bindings = bs) ->
                for b in bs do
                    iterBindingSigTypes ty (iterTypeMemberSig it) b
            | ClassFunctionOrValueDefn.Do _ -> ()

        let body (b: ObjectModelBody<SyntaxToken>) =
            match b.inherits with
            | ValueSome(ClassInheritsDecl(typ = t)) -> onInherit t
            | ValueNone -> ()

            for d in b.classPreamble do
                preamble d

            for el in b.elements do
                element el

        let extensions (ext: TypeExtensionElements<SyntaxToken> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = els)) ->
                for el in els do
                    element el
            | ValueNone -> ()

        let headerConstraints (tn: TypeName<SyntaxToken>) = iterTypeNameConstraints it tn

        match td with
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn) -> headerConstraints tn
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ()

        match td with
        | TypeDefn.Abbrev(typ = t; extensions = ext) ->
            ty t
            extensions ext
        | TypeDefn.Record(fields = fs; extensions = ext) ->
            for RecordField(typ = t) in fs do
                ty t

            extensions ext
        | TypeDefn.Union(cases = cs; extensions = ext) ->
            for c in cs do
                iterTypeUnionCase it c

            extensions ext
        | TypeDefn.Anon(primaryConstr = pc; body = b)
        | TypeDefn.Class(primaryConstr = pc; body = b)
        | TypeDefn.Struct(primaryConstr = pc; body = b) ->
            // Primary constructor parameters (e.g., `type Point(x: int)`) are represented as patterns.
            match pc with
            | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) -> onPat p
            | _ -> ()

            body b
        | TypeDefn.Interface(body = b) -> body b
        | TypeDefn.Delegate(sign = DelegateSig(sign = s)) -> iterTypeUncurriedSig it s
        | TypeDefn.TypeExtension(elements = TypeExtensionElements(elements = els)) ->
            for el in els do
                element el
        | TypeDefn.Enum _
        | TypeDefn.AbstractType _
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ()

    /// Traverses structural types in a signature file or interface specification (base classes,
    /// interface implementations, and `val` fields). Method and property members are handled separately.
    let iterTypeElementsSignatureStructure (it: ITypeVisitor) (els: TypeElementsSignature<SyntaxToken>) : unit =
        for el in els do
            match el with
            | TypeSignatureElement.Interface(InterfaceSpec.InterfaceSpec(typ = t))
            | TypeSignatureElement.Value(typ = t)
            | TypeSignatureElement.Inherit(ClassInheritsDecl(typ = t)) -> iterType it t
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Member _
            | TypeSignatureElement.Abstract _
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _
            | TypeSignatureElement.StaticMember _ -> ()

    /// Traverses all types and constraints declared in a `val` signature.
    let iterValSigTypes (it: ITypeVisitor) (ValSig(typars = tds; signature = cs)) : unit =
        match tds with
        | ValueSome(TyparDefns(constraints = ValueSome tcs)) -> iterTypeConstraints it tcs
        | _ -> ()

        iterTypeCurriedSig it cs

    /// Collects all type constraints associated with a definition, including both the header
    /// `when` constraints and any inline `Type.WhenConstrainedType` constraints.
    let whenClauses
        (header: TyparDefns<SyntaxToken> voption)
        (walk: ITypeVisitor -> unit)
        : TyparConstraints<SyntaxToken> list =
        let acc = ResizeArray<TyparConstraints<SyntaxToken>>()

        match header with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) -> acc.Add cs
        | _ -> ()

        walk (
            TypeVisitor.ofTypes (fun t ->
                match t with
                | Type.WhenConstrainedType(constraints = cs) -> acc.Add cs
                | _ -> ()

                true
            )
        )

        List.ofSeq acc
