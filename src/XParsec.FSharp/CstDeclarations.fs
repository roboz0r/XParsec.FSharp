namespace XParsec.FSharp.Parser

open System.Collections.Immutable

module CstDeclarations =

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
