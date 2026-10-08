namespace XParsec.FSharp.Parser

open XParsec.FSharp.Parser

/// Helper functions and active patterns for inspecting and classifying `TypeDefn` nodes.
///
/// Note: The parser produces `TypeDefn.Anon` for implicit class declarations that omit
/// `class ... end` (e.g., `type C() = member ...`).
module TypeDefnPatterns =

    /// Extracts the `TypeName` from any written `type` form, including type extensions.
    /// Returns `ValueNone` for recovery nodes, which have no name.
    let tryTypeName (td: TypeDefn<'T>) : TypeName<'T> voption =
        match td with
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn)
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn) -> ValueSome tn
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ValueNone

    [<NoEquality; NoComparison>]
    type ClassLikeDecl<'T> =
        {
            TypeName: TypeName<'T>
            PrimaryConstr: PrimaryConstrArgs<'T> voption
            AsDefn: AsDefn<'T> voption
            Body: ObjectModelBody<'T>
        }

    /// Matches definitions with an object-model body (`Class`, `Anon`, `Struct` and `Interface`) and
    /// extracts their common components. An `interface … end` definition has no primary constructor
    /// and no self identifier. Returns `ValueNone` for records, unions, abbreviations, etc.
    let tryClassLikeDecl (td: TypeDefn<'T>) : ClassLikeDecl<'T> voption =
        match td with
        | TypeDefn.Class(typeName = tn; primaryConstr = pc; asDefn = asD; body = body)
        | TypeDefn.Anon(typeName = tn; primaryConstr = pc; asDefn = asD; body = body)
        | TypeDefn.Struct(typeName = tn; primaryConstr = pc; asDefn = asD; body = body) ->
            ValueSome
                {
                    TypeName = tn
                    PrimaryConstr = pc
                    AsDefn = asD
                    Body = body
                }
        | TypeDefn.Interface(typeName = tn; body = body) ->
            ValueSome
                {
                    TypeName = tn
                    PrimaryConstr = ValueNone
                    AsDefn = ValueNone
                    Body = body
                }
        | _ -> ValueNone

    /// Returns true if the type explicitly uses `type X = struct ... end` syntax.
    ///
    /// Note: Types marked with a `[<Struct>]` attribute are parsed as `Class` or `Anon`
    /// and will return false here.
    let isStructShape (td: TypeDefn<'T>) : bool =
        match td with
        | TypeDefn.Struct _ -> true
        | _ -> false

    /// The `inherit` clauses of `body` in declaration order. A class body holds its leading clause in
    /// `inherits`; an interface body may hold several.
    let inheritClauses (body: ObjectModelBody<'T>) : ClassInheritsDecl<'T> list =
        [
            match body.inherits with
            | ValueSome decl -> decl
            | ValueNone -> ()

            for el in body.elements do
                match el with
                | TypeDefnElement.Inherit decl -> decl
                | _ -> ()
        ]

    /// Returns true if the body declares at least one abstract member or `inherit` clause, every
    /// member is abstract, every `inherit` clause is written without arguments, and it has no `let`
    /// or `do` preamble.
    let isAbstractOnlyBody (body: ObjectModelBody<'T>) : bool =
        let argless (ClassInheritsDecl(expr = arg)) = arg.IsNone

        body.classPreamble.IsEmpty
        && (body.inherits.IsSome || not body.elements.IsEmpty)
        && body.inherits |> ValueOption.forall argless
        && body.elements
           |> Seq.forall (fun el ->
               match el with
               | TypeDefnElement.Member(MemberDefn.Member(defn = MethodOrPropDefn.AbstractSignature _)) -> true
               | TypeDefnElement.Inherit decl -> argless decl
               | _ -> false
           )

    /// Signature-file (`.fsi`) equivalent of `isAbstractOnlyBody`.
    ///
    /// Returns true if the signature elements contain at least one abstract member and no
    /// concrete members, values, or constructors. (`inherit` and `interface` clauses are ignored).
    let bodyIsInterface (elems: TypeElementsSignature<'T>) : bool =
        let mutable hasAbstract = false
        let mutable hasConcrete = false

        for e in elems do
            match e with
            | TypeSignatureElement.Abstract _ -> hasAbstract <- true
            | TypeSignatureElement.Member _
            | TypeSignatureElement.StaticMember _
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Value _
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _ -> hasConcrete <- true
            | TypeSignatureElement.Interface _
            | TypeSignatureElement.Inherit _ -> ()

        hasAbstract && not hasConcrete

    /// Extracts the type name and any `with ...` augmentation members for single-identifier
    /// records, unions, and intrinsic type abbreviations.
    let tryNonClassMemberHostDecl (td: TypeDefn<'T>) : struct (TypeName<'T> * TypeDefnElements<'T> voption) voption =
        let extElems (ext: TypeExtensionElements<'T> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = elems)) -> ValueSome elems
            | ValueNone -> ValueNone

        match td with
        | TypeDefn.Union(typeName = (TypeName(ident = nameLi) as tn); extensions = ext)
        | TypeDefn.Record(typeName = (TypeName(ident = nameLi) as tn); extensions = ext) when nameLi.Idents.Length = 1 ->
            ValueSome(struct (tn, extElems ext))
        // Intrinsic type abbreviations with `with member ...` augmentations:
        // e.g. `type X = (# ... #) with member ...`
        | TypeDefn.Abbrev(typeName = (TypeName(ident = nameLi) as tn); extensions = ext & ValueSome _) when
            nameLi.Idents.Length = 1
            ->
            ValueSome(struct (tn, extElems ext))
        | _ -> ValueNone

    /// Extracts the `ObjectModelBody` from a `Class`, `Anon`, `Struct`, or `Interface` definition.
    let tryObjectModelBody (td: TypeDefn<'T>) : ObjectModelBody<'T> voption =
        match td with
        | TypeDefn.Class(body = b)
        | TypeDefn.Anon(body = b)
        | TypeDefn.Struct(body = b)
        | TypeDefn.Interface(body = b) -> ValueSome b
        | _ -> ValueNone
