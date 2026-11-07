namespace XParsec.FSharp.Parser

open System.Collections.Immutable

/// The inlining written on a `let` binding.
[<RequireQualifiedAccess>]
type BindingInlining =
    | NotInline
    /// `inline` on a binding with parameters or explicit type parameters.
    | Inline
    /// `inline` on a binding with neither parameters nor explicit type parameters, which F# refuses
    /// (FS0832).
    | InlineValue

module CstDeclarations =

    let inlining (binding: Binding<'T>) : BindingInlining =
        match binding.inlineToken, binding.typarDefns, binding.argumentPats.Length with
        | ValueNone, _, _ -> BindingInlining.NotInline
        | ValueSome _, ValueNone, 0 -> BindingInlining.InlineValue
        | ValueSome _, _, _ -> BindingInlining.Inline

    /// Whether `binding` is inlined at its uses: an `Inline` binding.
    let isInlined (binding: Binding<'T>) : bool =
        inlining binding = BindingInlining.Inline

    /// The attributes that apply to `binding`, the binding at `index` of a `let` group whose
    /// declaration carries the `leading` attributes written before `let`. The leading attributes
    /// apply to the first binding, ahead of its own.
    let bindingAttributes
        (leading: Attributes<'T> voption)
        (index: int)
        (binding: Binding<'T>)
        : Attributes<'T> voption =
        match index, leading, binding.attributes with
        | 0, ValueSome leading, ValueSome own -> ValueSome(leading.AddRange own)
        | 0, ValueSome leading, ValueNone -> ValueSome leading
        | _, _, own -> own

    /// Flattens all declarations in an implementation file into a single list in source order,
    /// unpacking nested module bodies.
    let implFileElems (file: ImplementationFile<SyntaxToken>) : ModuleElems<SyntaxToken> =
        let b = ImmutableArray.CreateBuilder<ModuleElem<SyntaxToken>>()

        let rec add (elems: ModuleElems<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleElem.Module(ModuleDefn.ModuleDefn(body = ModuleDefnBody(elements = inner))) ->
                    match inner with
                    | ValueSome innerElems -> add innerElems
                    | ValueNone -> ()
                | _ -> b.Add e

        match file with
        | ImplementationFile.AnonymousModule elems -> add elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> add elems
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(elements = elems)
                | NamespaceDeclGroup.Global(elements = elems) -> add elems

        b.ToImmutable()
