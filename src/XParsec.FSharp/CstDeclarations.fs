namespace XParsec.FSharp.Parser

open System.Collections.Immutable

module CstDeclarations =

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
