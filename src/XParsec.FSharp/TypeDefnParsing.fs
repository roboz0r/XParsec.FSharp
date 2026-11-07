namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState


[<RequireQualifiedAccess>]
module PrimaryConstrArgs =
    let parse: Parser<PrimaryConstrArgs<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt Access.parse
            let! lParen = pLParen
            let! pat = opt Pat.parse
            let! rParen = pRParen
            return PrimaryConstrArgs(attrs, access, lParen, pat, rParen)
        }

[<RequireQualifiedAccess>]
module TypeName =
    /// Parse ML-style prefix type parameters: 'T or ('a, 'b)
    let private pPrefixTypars: Parser<PrefixTypars<SyntaxToken>, _, _, _> =
        choiceL
            [
                // ('a, 'b) — multiple prefix typars
                parser {
                    let! lParen = pLParen
                    let! typars, commas = sepBy1 Typar.parse pComma
                    let! rParen = pRParen
                    return PrefixTypars.Multiple(lParen, typars, commas, rParen)
                }
                // 'T — single prefix typar
                Typar.parse |>> PrefixTypars.Single
            ]
            "Prefix type parameters"

    /// Parses a TypeName using pre-parsed attributes (from before the `type` keyword).
    let parseWithAttrs (attrs: Attributes<SyntaxToken> voption) : Parser<TypeName<SyntaxToken>, _, _, _> =
        parser {
            let! access = opt Access.parse
            let! prefixTypars = opt pPrefixTypars
            let! ident = LongIdent.parse
            let! typars = opt TyparDefns.parse

            // Parse optional postfix 'when' constraints (outside angle brackets):
            // type Set<'T, 'Tag> when 'Tag :> IComparer<'T> = ...
            let! postfixConstraints = opt TyparConstraints.parse

            return TypeName(attrs, access, prefixTypars, ident, typars, postfixConstraints)
        }

[<RequireQualifiedAccess>]
module AsDefn =
    let parse: Parser<AsDefn<SyntaxToken>, _, _, _> =
        parser {
            let! asTok = pAs
            let! ident = pIdent
            return AsDefn(asTok, ident)
        }

// ----------------------------------------------------------------------------
// Signatures (MemberSig, ArgSpec)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ArgNameSpec =
    let parse =
        parser {
            let! optional = opt pQuestionMark
            let! ident = pIdent
            let! colon = pColon
            return ArgNameSpec.ArgNameSpec(optional, ident, colon)
        }

[<RequireQualifiedAccess>]
module ArgSpec =
    let parse =
        parser {
            let! attrs = opt Attributes.parse
            // Try parse "name :" first
            let! nameSpec = opt ArgNameSpec.parse
            // Use parseArgField (allows `T | null`) not parseField: * separates args,
            // -> separates arg groups, `|` inside the type denotes a nullable union.
            let! typ = Type.parseArgField
            return ArgSpec.ArgSpec(attrs, nameSpec, typ)
        }


[<RequireQualifiedAccess>]
module ArgsSpec =
    let parse =
        sepBy1 ArgSpec.parse pOpMultiply
        |>> fun struct (args, asterisks) -> ArgsSpec.ArgsSpec(args, asterisks)

[<RequireQualifiedAccess>]
module CurriedSig =

    let private pArgsSpec = ArgsSpec.parse .>>. pArrowRight

    let parse: Parser<CurriedSig<SyntaxToken>, _, _, _> =
        parser {
            // Use `many` (not `many1Till`) so properties with zero `->` are handled.
            // For each arg group, greedily try pArgsSpec (args -> ), stop when it fails.
            // The remaining is the return type.
            let! args = many pArgsSpec
            let! ret = Type.parse
            return CurriedSig(args, ret)
        }

    do refCurriedSig.Set parse

[<RequireQualifiedAccess>]
module UncurriedSig =

    let parse: Parser<UncurriedSig<SyntaxToken>, _, _, _> =
        parser {
            let! args = ArgsSpec.parse
            let! arrow = pArrowRight
            let! retType = Type.parse
            return UncurriedSig.UncurriedSig(args, arrow, retType)
        }

/// Signature for an abstract member (method or property)
[<RequireQualifiedAccess>]
module MemberSig =

    (*
member-sig :=
    ident typar-defns~opt : curried-sig -- method or property signature
    ident typar-defns~opt : curried-sig with get -- property signature
    ident typar-defns~opt : curried-sig with set -- property signature
    ident typar-defns~opt : curried-sig with get,set -- property signature
    ident typar-defns~opt : curried-sig with set,get -- property signature
*)

    let parse: Parser<MemberSig<SyntaxToken>, _, _, _> =
        parser {
            // IdentOrOp so `static member (+): T * T -> T` parses; covers plain idents too.
            let! ident = IdentOrOp.parse
            let! typars = opt TyparDefns.parse
            let! colon = pColon
            let! sigType = CurriedSig.parse

            // Check for optional 'with' get/set
            let! withClause = opt WithClause.parse

            match withClause with
            | ValueSome clause -> return MemberSig.PropSig(ident, typars, colon, sigType, clause)
            | ValueNone -> return MemberSig.MethodOrPropSig(ident, typars, colon, sigType)
        }


// ----------------------------------------------------------------------------
// Member Definitions (Method, Property, Ctor)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module MethodOrPropDefn =

    let private errExpectedDotAfterUnderscore: ErrorType<PositionedToken, ParseState> =
        Message "Expected '.' after '_' in member definition"

    let private errExpectedGetOrSet: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'get' or 'set'"

    // Distinguishes:
    // member x.P = ... (Property)
    // member x.M args = ... (Method)
    // member x.P with get ... (PropWithGetSet)

    /// The placeholder a rejected `get` / `set` binding leaves behind, holding its attributes and
    /// the tokens skipped past it.
    let private skippedGetSetBinding
        (attrs: Attributes<SyntaxToken> voption)
        (at: SyntaxToken)
        (skipped: ImArr<SyntaxToken>)
        : Binding<SyntaxToken> =
        {
            attributes = attrs
            inlineToken = ValueNone
            mutableToken = ValueNone
            access = ValueNone
            pattern = missingOrSkipped Pat.Missing Pat.SkipsTokens skipped
            typarDefns = ValueNone
            argumentPats = ImmutableArray.Empty
            returnType = ValueNone
            equals = virtualToken (mkVirtualPT Token.OpEquality at.PositionedToken.StartIndex)
            expr = Expr.Missing
        }

    /// Peeks past an optional access modifier to the `get`/`set` token that begins an accessor binding.
    let private peekAccessorName =
        lookAhead (
            parser {
                let! _ = opt Access.parse
                return! peekNextSyntaxToken
            }
        )

    /// `member x.P with get … and set …`. A property written without a `with`
    /// clause is an implicit get, so a third name is rejected here and the tree has
    /// only `get` and `set` in that position.
    let private pGetSetBinding (propIdent: SyntaxToken) attrs : FSParser<Binding<SyntaxToken>> =
        parser {
            let! state = getUserState
            let! next = peekAccessorName

            if tokenStringIs "get" next state || tokenStringIs "set" next state then
                return! Binding.parse attrs
            else
                // A virtual token has no source text, so print its kind instead.
                let shown =
                    match tokenString next state with
                    | "" -> string next.Token
                    | text -> text

                let msg =
                    sprintf
                        "Expected 'get' or 'set' after 'with' on '%s' but got '%s'"
                        (tokenString propIdent state)
                        shown

                return!
                    recoverWith
                        StoppingTokens.afterGetSetBinding
                        (DiagnosticCode.Other msg)
                        (skippedGetSetBinding attrs next)
                        (fail errExpectedGetOrSet)
        }

    let pPropertyWithGetSet =
        parser {
            let! ident = pIdent
            let! w = pWith
            let! bindings, ands = Binding.sepByAnd1 (pGetSetBinding ident)
            return fun thisIdent -> MethodOrPropDefn.PropertyWithGetSet(thisIdent, ident, w, bindings, ands)
        }

    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _> =
        let buildMethodOrProp thisIdent (binding: Binding<SyntaxToken>) =
            if binding.argumentPats.IsEmpty then
                MethodOrPropDefn.Property(thisIdent, binding)
            else
                MethodOrPropDefn.Method(thisIdent, binding)

        parser {
            match! peekNextSyntaxToken with
            | tWild when tWild.Token = Token.Wildcard ->
                // `_` as self-identifier: must be followed by `.`
                let! underscore = consumePeeked tWild

                match! peekNextSyntaxToken with
                | tDot when tDot.Token = Token.OpDot ->
                    let! dot = consumePeeked tDot
                    let thisIdent = ValueSome struct (underscore, dot)

                    return!
                        choiceL
                            [
                                Binding.parse ValueNone |>> buildMethodOrProp thisIdent
                                pPropertyWithGetSet |>> fun f -> f thisIdent
                            ]
                            "MethodOrPropDefn after _."

                | _ -> return! fail errExpectedDotAfterUnderscore

            | _ ->

                match! opt pIdent with
                | ValueSome ident ->
                    match! peekNextSyntaxToken with
                    | t when t.Token = Token.OpDot ->
                        let! dot = consumePeeked t
                        let thisIdent = ValueSome struct (ident, dot)

                        return!
                            choiceL
                                [
                                    Binding.parse ValueNone |>> buildMethodOrProp thisIdent
                                    pPropertyWithGetSet |>> fun f -> f thisIdent
                                ]
                                "MethodOrPropDefn after dot"

                    | t when t.Token = Token.OpEquality ->
                        let! eq = consumePeeked t
                        // Grammar: member body is typedSeqExprBlock
                        let! expr = refTypedSeqExprBlock.Parser

                        let binding =
                            {
                                attributes = ValueNone
                                inlineToken = ValueNone
                                mutableToken = ValueNone
                                access = ValueNone
                                pattern = Pat.NamedSimple ident
                                typarDefns = ValueNone
                                argumentPats = ImmutableArray.Empty
                                returnType = ValueNone
                                equals = eq
                                expr = expr
                            }

                        return MethodOrPropDefn.Property(ValueNone, binding)

                    | t when t.Token = Token.OpColon ->
                        // Property with explicit return type (e.g., static member Empty : Foo = expr)
                        let! returnType = ReturnType.parse
                        let! equals = pEquals
                        // Grammar: member body is typedSeqExprBlock
                        let! expr = refTypedSeqExprBlock.Parser

                        let binding =
                            {
                                attributes = ValueNone
                                inlineToken = ValueNone
                                mutableToken = ValueNone
                                access = ValueNone
                                pattern = Pat.NamedSimple ident
                                typarDefns = ValueNone
                                argumentPats = ImmutableArray.Empty
                                returnType = ValueSome returnType
                                equals = equals
                                expr = expr
                            }

                        return MethodOrPropDefn.Property(ValueNone, binding)

                    | t when t.Token = Token.KWWith ->
                        // A property member whose ident was already consumed above
                        // (`static member BuildPhase with get () = … and set v = …`).
                        let! w = consumePeeked t
                        let! bindings, ands = Binding.sepByAnd1 (pGetSetBinding ident)
                        return MethodOrPropDefn.PropertyWithGetSet(ValueNone, ident, w, bindings, ands)

                    | _ ->
                        // No self-identifier prefix (e.g., static member Create(args) = body)
                        // The ident we consumed is the method/property name itself.
                        return!
                            choiceL
                                [
                                    parser {
                                        let! typarDefns = opt TyparDefns.parse
                                        let! argumentPats = Pat.parseAtomicBindingArgMany1
                                        let! returnType = opt ReturnType.parse
                                        let! equals = pEquals
                                        // Grammar: member body is typedSeqExprBlock
                                        let! expr = refTypedSeqExprBlock.Parser

                                        let binding =
                                            {
                                                attributes = ValueNone
                                                inlineToken = ValueNone
                                                mutableToken = ValueNone
                                                access = ValueNone
                                                pattern = Pat.NamedSimple ident
                                                typarDefns = typarDefns
                                                argumentPats = argumentPats
                                                returnType = returnType
                                                equals = equals
                                                expr = expr
                                            }

                                        return MethodOrPropDefn.Method(ValueNone, binding)
                                    }
                                    pPropertyWithGetSet |>> fun f -> f ValueNone
                                ]
                                "MethodOrPropDefn no self-ident"
                | ValueNone ->
                    return!
                        choiceL
                            [
                                Binding.parse ValueNone |>> buildMethodOrProp ValueNone
                                pPropertyWithGetSet |>> fun f -> f ValueNone
                            ]
                            "MethodOrPropDefn no ident"
        }

[<RequireQualifiedAccess>]
module AutoPropDefn =
    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _> =
        parser {
            let! valTok = pVal
            let! access = opt Access.parse
            let! ident = pIdent

            let! returnType =
                opt (
                    parser {
                        let! colon = pColon
                        let! t = Type.parse
                        return ReturnType(colon, t)
                    }
                )

            let! eq = pEquals
            let! expr = Expr.parse

            // Handle post-expression type annotation: `val X = expr : Type with get, set`
            // The expression parser doesn't consume `:` at top level, so we handle it here.
            let! colonOpt = opt peekNextSyntaxToken

            let! expr =
                match colonOpt with
                | ValueSome t when t.Token = Token.OpColon ->
                    parser {
                        let! colon = consumePeeked t
                        let! typ = Type.parse
                        return Expr.TypeAnnotation(expr, colon, typ)
                    }
                | _ -> preturn expr

            let! withClause = opt WithClause.parse
            return MethodOrPropDefn.AutoProperty(valTok, access, ident, returnType, eq, expr, withClause)
        }

[<RequireQualifiedAccess>]
module MemberDefn =
    let private errExpectedMemberOrValAfterStatic: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'member', 'abstract' or 'val' after 'static'"

    // Ths spec has incorrect grammar for member definitions, so we need to
    // reverse-engineer it from examples and the F# spec text.
    // It seems like the grammar listing in the individual subsections are closed to correct than the
    // overall grammar in https://fsharp.github.io/fslang-spec/type-definitions/ introduction.
    let private pStaticMemberDefn =
        parser {
            let! staticTok = pStatic
            let! t = peekNextSyntaxToken

            match t.Token with
            | Token.KWMember ->
                let! mem = consumePeeked t
                let! inlineTok = opt pInline

                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWVal ->
                    let! defn = AutoPropDefn.parse

                    return
                        (fun attrs ->
                            MemberDefn.Member(
                                attrs,
                                ValueSome staticTok,
                                MemberKeyword.Member mem,
                                inlineTok,
                                ValueNone,
                                defn
                            )
                        )
                | _ ->
                    let! access = opt Access.parse
                    let! defn = MethodOrPropDefn.parse

                    return
                        (fun attrs ->
                            MemberDefn.Member(
                                attrs,
                                ValueSome staticTok,
                                MemberKeyword.Member mem,
                                inlineTok,
                                access,
                                defn
                            )
                        )
            | Token.KWAbstract ->
                let! abstractTok = consumePeeked t
                let! memTok = opt pMember
                let! access = opt Access.parse
                let! sigDef = MemberSig.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueSome staticTok,
                            MemberKeyword.Abstract(abstractTok, memTok),
                            ValueNone,
                            access,
                            MethodOrPropDefn.AbstractSignature sigDef
                        )
                    )
            | Token.KWVal ->
                let! valTok = consumePeeked t
                let! mut = opt pMutable
                let! access = opt Access.parse
                let! ident = pIdent
                let! colon = pColon
                let! typ = Type.parse

                return
                    (fun attrs -> MemberDefn.Value(attrs, ValueSome staticTok, valTok, mut, access, ident, colon, typ))
            | _ -> return! fail errExpectedMemberOrValAfterStatic
        }

    let private pMemberDefn =
        parser {
            let! memberTok = pMember
            let! inlineTok = opt pInline

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(attrs, ValueNone, MemberKeyword.Member memberTok, inlineTok, ValueNone, defn)
                    )
            | _ ->
                let! access = opt Access.parse
                let! defn = MethodOrPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(attrs, ValueNone, MemberKeyword.Member memberTok, inlineTok, access, defn)
                    )
        }

    let private pAbstractMemberDefn =
        parser {
            let! abstractTok = pAbstract
            let! memTok = opt pMember
            let! access = opt Access.parse
            let! sigDef = MemberSig.parse

            return
                (fun attrs ->
                    MemberDefn.Member(
                        attrs,
                        ValueNone,
                        MemberKeyword.Abstract(abstractTok, memTok),
                        ValueNone,
                        access,
                        MethodOrPropDefn.AbstractSignature sigDef
                    )
                )
        }

    let private pOverrideMemberDefn =
        parser {
            let! overrideTok = pOverride

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueNone,
                            MemberKeyword.Override overrideTok,
                            ValueNone,
                            ValueNone,
                            defn
                        )
                    )
            | _ ->
                let! access = opt Access.parse
                let! defn = MethodOrPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueNone,
                            MemberKeyword.Override overrideTok,
                            ValueNone,
                            access,
                            defn
                        )
                    )
        }

    let private pDefaultMemberDefn =
        parser {
            let! defaultTok = pDefault

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWVal ->
                let! defn = AutoPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(
                            attrs,
                            ValueNone,
                            MemberKeyword.Default defaultTok,
                            ValueNone,
                            ValueNone,
                            defn
                        )
                    )
            | _ ->
                let! access = opt Access.parse
                let! defn = MethodOrPropDefn.parse

                return
                    (fun attrs ->
                        MemberDefn.Member(attrs, ValueNone, MemberKeyword.Default defaultTok, ValueNone, access, defn)
                    )
        }

    let private pValueMemberDefn =
        parser {
            let! valTok = pVal
            let! mut = opt pMutable
            let! access = opt Access.parse
            let! ident = pIdent
            let! colon = pColon
            let! typ = Type.parse
            return (fun attrs -> MemberDefn.Value(attrs, ValueNone, valTok, mut, access, ident, colon, typ))
        }


    let private pAdditionalConstrDefn =
        parser {
            let! access = opt Access.parse
            let! newTok = pNew
            let! pat = Pat.parse
            let! asDefn = opt AsDefn.parse
            let! equals = pEquals
            let! body = refTypedSeqExprBlock.Parser

            let shape = ConstructorBody.ofExpr body

            return
                (fun attrs -> MemberDefn.AdditionalConstructor(attrs, access, newTok, pat, asDefn, equals, body, shape))
        }

    let private memberDefnDispatcher =
        dispatchNextSyntaxTokenFallback
            [
                Token.KWMember, pMemberDefn
                Token.KWStatic, pStaticMemberDefn
                Token.KWOverride, pOverrideMemberDefn
                Token.KWVal, pValueMemberDefn
                Token.KWAbstract, pAbstractMemberDefn
                Token.KWDefault, pDefaultMemberDefn
            ]
            pAdditionalConstrDefn

    // Implementation of the forward reference stub
    let parse: Parser<MemberDefn<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse

            return! memberDefnDispatcher |>> fun memberDefnBuilder -> memberDefnBuilder attrs
        }

    // Set the forward reference in your existing framework
    do refMemberDefn.Set parse

// ----------------------------------------------------------------------------
// Type Body Elements
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module TypeDefnElement =
    let parse: Parser<TypeDefnElement<SyntaxToken>, _, _, _> =
        choiceL
            [
                parser {
                    let! intf = pInterface
                    let! t = Type.parse

                    // 'interface IFoo with ...' (implementation) vs 'interface IFoo' (specification)
                    let! withTok = opt pWith

                    match withTok with
                    | ValueSome wTok ->
                        let! members = withContext OffsideContext.WithAugment (many refMemberDefn.Parser)

                        // The AST requires an 'end' token for ObjectMembers, but interface blocks
                        // do not have their own 'end'. Synthesize a virtual one at the next token's position.
                        //
                        // Note: Using `opt` avoids failing/backtracking if we are at EOF or an offside boundary.
                        let! nextTokOpt = opt peekNextSyntaxToken

                        let virtualEnd =
                            {
                                PositionedToken =
                                    mkVirtualPT
                                        Token.KWEnd
                                        (match nextTokOpt with
                                         | ValueSome tok -> tok.StartIndex
                                         | ValueNone -> 0)
                                Index = TokenIndex.Virtual
                            }

                        let objMembers = ObjectMembers.ObjectMembers(wTok, members, virtualEnd)

                        return TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(intf, t, ValueSome objMembers))
                    | ValueNone -> return TypeDefnElement.InterfaceSpec(InterfaceSpec.InterfaceSpec(intf, t))
                }
                parser {
                    let! inh = pInherit
                    let! t = Type.parse
                    let! e = opt Expr.parseInheritArg
                    return TypeDefnElement.Inherit(ClassInheritsDecl.ClassInheritsDecl(inh, t, e))
                }
                MemberDefn.parse |>> TypeDefnElement.Member
            ]
            "Type Definition Element"

[<RequireQualifiedAccess>]
module TypeDefnElements =
    // Parses a list of elements until 'end' or other terminator
    let parseTill terminator =
        manyTill TypeDefnElement.parse terminator

    /// Parses elements using offside rule (many stops when indentation drops).
    /// Use inside a withContext block.
    let parseMany = many TypeDefnElement.parse

// ----------------------------------------------------------------------------
// Specific Type Bodies (Class, Union, Record, etc.)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ClassInheritsDecl =
    let parse: Parser<ClassInheritsDecl<SyntaxToken>, _, _, _> =
        parser {
            let! inh = pInherit
            let! t = Type.parse
            let! e = opt Expr.parseInheritArg
            return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
        }

[<RequireQualifiedAccess>]
module ClassFunctionOrValueDefn =
    let parse: Parser<ClassFunctionOrValueDefn<SyntaxToken>, _, _, _> =
        choiceL
            [
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt pStatic
                    let! d = pDo
                    // Grammar: DO typedSeqExprBlock
                    let! e = refTypedSeqExprBlock.Parser
                    return ClassFunctionOrValueDefn.Do(attrs, stat, d, e)
                }
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt pStatic
                    let! l = pLet
                    let! r = opt pRec
                    let! bindings, ands = Binding.parseSepByAnd1
                    return ClassFunctionOrValueDefn.LetBindings(attrs, stat, l, r, bindings, ands)
                }
            ]
            "Class Let/Do"

/// Parses the body of a class, struct, interface, or object expression.
[<RequireQualifiedAccess>]
module ClassTypeBody =
    let private errExpectedEndOfTypeBody: ErrorType<PositionedToken, ParseState> =
        Message "Expected end of type body"

    /// Parses an explicit class body delimited by an end token (e.g. `class ... end` or `{ new ... with ... }`).
    ///
    /// Returns the parsed body and the terminating token.
    ///
    /// Note: F# allows `let`/`do` bindings and member definitions to be mixed
    /// (e.g. static members appearing before instance `let` bindings), so each
    /// loop iteration tries to parse either a preamble binding or a type member.
    let parse terminator : Parser<ObjectModelBody<SyntaxToken> * SyntaxToken, _, _, _> =
        fun reader ->
            let inh =
                match (opt ClassInheritsDecl.parse) reader with
                | Ok v -> v
                | Error _ -> ValueNone

            let preamble = ResizeArray<_>()
            let elems = ResizeArray<_>()
            let mutable keepGoing = true
            let mutable endResult = ValueNone

            while keepGoing do
                match terminator reader with
                | Ok endTok ->
                    endResult <- ValueSome endTok
                    keepGoing <- false
                | Error _ ->
                    match ClassFunctionOrValueDefn.parse reader with
                    | Ok p -> preamble.Add(p)
                    | Error _ ->
                        match TypeDefnElement.parse reader with
                        | Ok e -> elems.Add(e)
                        | Error _ -> keepGoing <- false

            match endResult with
            | ValueSome endTok ->
                Ok(
                    {
                        inherits = inh
                        classPreamble = ImmutableArray.CreateRange(preamble)
                        elements = ImmutableArray.CreateRange(elems)
                    },
                    endTok
                )
            | ValueNone -> fail errExpectedEndOfTypeBody reader

    /// Parses an indentation-based (offside) class body (e.g. `type Foo() =`).
    ///
    /// Must be called within an indentation-sensitive context (`withContext`).
    /// Stops when no further bindings or members can be parsed at the current indentation level.
    let parseOffside: Parser<ObjectModelBody<SyntaxToken>, _, _, _> =
        fun reader ->
            let inh =
                match (opt ClassInheritsDecl.parse) reader with
                | Ok v -> v
                | Error _ -> ValueNone

            let preamble = ResizeArray<_>()
            let elems = ResizeArray<_>()
            let mutable keepGoing = true

            while keepGoing do
                match ClassFunctionOrValueDefn.parse reader with
                | Ok p -> preamble.Add(p)
                | Error _ ->
                    match TypeDefnElement.parse reader with
                    | Ok e -> elems.Add(e)
                    | Error _ -> keepGoing <- false

            Ok
                {
                    inherits = inh
                    classPreamble = ImmutableArray.CreateRange(preamble)
                    elements = ImmutableArray.CreateRange(elems)
                }

/// Parses the body of a struct definition (no inherits or class preamble).
[<RequireQualifiedAccess>]
module StructTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<ObjectModelBody<SyntaxToken> * SyntaxToken, _, _, _> =
        parser {
            let! elems, endTok = TypeDefnElements.parseTill terminator

            let body =
                {
                    inherits = ValueNone
                    classPreamble = ImmutableArray.Empty
                    elements = elems
                }

            return (body, endTok)
        }

/// Parses the body of an interface definition (no inherits or class preamble).
[<RequireQualifiedAccess>]
module InterfaceTypeBody =
    // Returns the body AND the consumed end token (from manyTill's terminator)
    let parse terminator : Parser<ObjectModelBody<SyntaxToken> * SyntaxToken, _, _, _> =
        parser {
            let! elems, endTok = TypeDefnElements.parseTill terminator

            let body =
                {
                    inherits = ValueNone
                    classPreamble = ImmutableArray.Empty
                    elements = elems
                }

            return (body, endTok)
        }

// --- Union ---

[<RequireQualifiedAccess>]
module UnionTypeField =
    let parse: Parser<UnionTypeField<SyntaxToken>, _, _, _> =
        // In union case field lists, `*` is a FIELD SEPARATOR, not a tuple operator.
        // So each individual field type must be parsed without tuple handling.
        choiceL
            [
                parser {
                    let! ident = pIdent
                    let! colon = pColon
                    let! t = Type.parseField
                    return UnionTypeField.Named(ident, colon, t)
                }
                parser {
                    let! t = Type.parseField
                    return UnionTypeField.Unnamed t
                }
            ]
            "Union field"

[<RequireQualifiedAccess>]
module UnionTypeCaseData =
    let parseFields = sepBy1 UnionTypeField.parse pOpMultiply

    // Case name: plain identifier, (op), (*), or ([]). IdentOrOp.parse covers all.
    let private pCaseName = IdentOrOp.parse

    // | Name of field * field ...
    let private parseNaryOf: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        parser {
            let! name = pCaseName
            let! ofTok = pOf
            let! fields, asterisks = parseFields
            return UnionTypeCaseData.Nary(name, ofTok, fields, asterisks)
        }

    // | Name : arg -> retType  (GADT-style with args)
    // Uses Type.parseNoUnion for the return type so a following `| NextCase`
    // is not swallowed as a `T | T` nullable-ref type union.
    let private parseGadtNary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        parser {
            let! name = pCaseName
            let! colon = pColon
            let! args = ArgsSpec.parse
            let! arrow = pArrowRight
            let! retType = Type.parseNoUnion
            return UnionTypeCaseData.GadtNary(name, colon, UncurriedSig.UncurriedSig(args, arrow, retType))
        }

    // | Name : retType  (GADT-style nullary with explicit return type)
    let private parseGadtNullary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        parser {
            let! name = pCaseName
            let! colon = pColon
            let! t = Type.parseNoUnion
            return UnionTypeCaseData.GadtNullary(name, colon, t)
        }

    // Non-nullary forms (used by ExceptionDefn which falls back to its own Nullary path)
    let parseNary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        choiceL [ parseNaryOf; parseGadtNary; parseGadtNullary ] "Union Case (Nary)"

    let parse: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _> =
        choiceL
            [
                parseNaryOf
                parseGadtNary
                parseGadtNullary
                parser {
                    let! name = pCaseName
                    return UnionTypeCaseData.Nullary name
                }
            ]
            "Union Case"

[<RequireQualifiedAccess>]
module UnionTypeCase =
    let parse: Parser<UnionTypeCase<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! caseData = UnionTypeCaseData.parse
            return UnionTypeCase.UnionTypeCase(attrs, caseData)
        }

[<RequireQualifiedAccess>]
module UnionTypeCases =
    let parse =
        parser {
            let! firstBar = opt pBar
            let! cases, sepBars = sepBy1 UnionTypeCase.parse pBar

            let bars =
                match firstBar with
                | ValueSome b -> sepBars.Insert(0, b)
                | ValueNone -> sepBars

            return struct (cases, bars)
        }

// --- Enum ---

[<RequireQualifiedAccess>]
module EnumTypeCase =
    let parse: Parser<EnumTypeCase<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! id = nextSyntaxIdentifierLMsg "Enum Name"
            let! eq = pEquals
            let! c = withContext OffsideContext.SeqBlock refExpr.Parser
            return EnumTypeCase.EnumTypeCase(attrs, id, eq, c)
        }

[<RequireQualifiedAccess>]
module EnumTypeCases =
    let parse =
        parser {
            let! firstBar = opt pBar
            let! cases, sepBars = sepBy1 EnumTypeCase.parse pBar

            let bars =
                match firstBar with
                | ValueSome b -> sepBars.Insert(0, b)
                | ValueNone -> sepBars

            return struct (cases, bars)
        }

// --- Type Extensions ---

[<RequireQualifiedAccess>]
module TypeExtensionElements =
    /// Parses explicit type extensions/augmentations using the `with ... end` syntax.
    let parse: Parser<TypeExtensionElements<SyntaxToken>, _, _, _> =
        parser {
            let! withTok = pWith
            let! elems = withContext OffsideContext.WithAugment TypeDefnElements.parseMany
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, elems, endTok)
        }

    /// Parses implicit type extensions in indentation-aware (light) syntax,
    /// where members directly follow a type definition without a `with` keyword.
    let parseLight (anchor: SyntaxToken) : Parser<TypeExtensionElements<SyntaxToken>, _, _, _> =
        // The members' offside line is `anchor`, the first token of the definition body
        // (`TypeDefn.BodyStart.Anchor` for a record or union).
        parser {
            let! withTok = nextSyntaxTokenVirtualIfNot Token.KWWith
            let! elems = withContextAtToken OffsideContext.WithAugment anchor (many1 TypeDefnElement.parse)
            let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
            return TypeExtensionElements.TypeExtensionElements(withTok, elems, endTok)
        }

    /// Parses an optional type extension, trying explicit `with` syntax first,
    /// then falling back to implicit light syntax anchored to the type definition.
    let parseOptAt (anchor: SyntaxToken) =
        opt (choiceL [ parse; parseLight anchor ] "Type Extension")

[<RequireQualifiedAccess>]
module DelegateSig =
    let parse: Parser<DelegateSig<SyntaxToken>, _, _, _> =
        parser {
            let! del = pDelegate
            let! ofTok = pOf
            let! sign = UncurriedSig.parse
            return DelegateSig.DelegateSig(del, ofTok, sign)
        }


// ----------------------------------------------------------------------------
// Top Level TypeDefn
// ----------------------------------------------------------------------------
[<RequireQualifiedAccess>]
module TypeDefn =

    let private errNotImplicit: ErrorType<PositionedToken, ParseState> =
        Message "Not implicit"

    let private errRetryAsMeasureType: ErrorType<PositionedToken, ParseState> =
        Message "Retry as measure type"

    let private errSingleNullaryUnionCaseIsAbbrev: ErrorType<PositionedToken, ParseState> =
        Message "Single nullary union case is a type abbreviation"

    /// Parses the right-hand side of a type abbreviation or measure expression.
    ///
    /// First parses a standard type; if immediately followed by measure operators
    /// (`/`, `*`, `^`, or `^-N`), it fails and retries as a measure expression.
    let parseAbbrevRhs: Parser<Type<SyntaxToken>, _, _, _> =
        choice
            [
                parser {
                    let! tNormal = Type.parse
                    // At EOF or a block dedent, no operator follows so treat as a standard type.
                    let! peekAfter = opt peekNextSyntaxToken
                    let! state = getUserState

                    match peekAfter with
                    | ValueNone -> return tNormal
                    | ValueSome peekAfter ->
                        // Match measure operators, including fused negative exponents (e.g. `^-1`).
                        let startsWithCaret = ParseState.tokenStringStartsWith "^" peekAfter state

                        if
                            peekAfter.Token = Token.OpDivision
                            || peekAfter.Token = Token.OpConcatenate
                            || peekAfter.Token = Token.OpMultiply
                            || startsWithCaret
                        then
                            return! fail errRetryAsMeasureType
                        else
                            return tNormal
                }
                (Measure.parse |>> Type.MeasureType)
            ]

    /// Parses a record field of a type definition or signature. The field type ends before a
    /// token in the field's start column, which begins the next field.
    let pRecordField =
        parser {
            let! fieldStart = peekNextSyntaxToken
            let! attrs = opt Attributes.parse
            let! mut = opt pMutable
            let! acc = opt Access.parse
            let! id = pIdent
            let! col = pColon
            let! t = withContextAtToken OffsideContext.SeqBlock fieldStart Type.parse
            let! _ = opt pSemi
            return RecordField.RecordField(attrs, mut, acc, id, col, t)
        }

    /// Parses the optional access modifier on a type representation (e.g. `type R = private { ... }`).
    let pRepresentationAccess = opt Access.parse

    /// The leading tokens of a type body after `=`.
    type BodyStart =
        {
            /// Selects the body kind. For a record or union representation this is the `{` or `|`
            /// after any access modifier; otherwise it is the first token, which the abbreviation
            /// branch consumes and diagnoses when it is a modifier.
            Kind: SyntaxToken
            /// The first token after `=`: the offside line for light-syntax members.
            Anchor: SyntaxToken
        }

    /// Peeks the leading tokens of a type body after `=`.
    let pBodyStart: FSParser<BodyStart> =
        parser {
            let! atModifier = peekNextSyntaxToken

            let! pastModifier =
                lookAhead (
                    parser {
                        let! _ = opt Access.parse
                        return! peekNextSyntaxToken
                    }
                )

            let kind =
                match pastModifier.Token with
                | Token.KWLBrace
                | Token.OpBar -> pastModifier
                | _ -> atModifier

            return { Kind = kind; Anchor = atModifier }
        }

    /// Disambiguates between an implicit class definition (e.g., `type Foo() = member ...`)
    /// and a type abbreviation (e.g., `type Foo = int`).
    let private parseAbbrevOrImplicitClass
        typeName
        (primaryConstr: PrimaryConstrArgs<SyntaxToken> voption)
        (asDefn: AsDefn<SyntaxToken> voption)
        equals
        =
        parser {
            // Identified as an implicit class if it has a constructor/self-identifier
            // or starts with a class-body keyword (e.g., member, val, inherit).
            let! isImplicitClass =
                lookAhead (
                    choiceL
                        [
                            pMember
                            pVal
                            pNew
                            pInherit
                            pAbstract
                            pDefault
                            pOverride
                            pStatic
                            // Handle member-level attributes (e.g., [<FieldOffset(0)>] val ...)
                            parser {
                                let! _ = Attributes.parse

                                return!
                                    choiceL
                                        [ pMember; pVal; pNew; pAbstract; pDefault; pOverride; pStatic ]
                                        "Attributed class keyword"
                            }
                            (if primaryConstr.IsSome || asDefn.IsSome then
                                 preturn (Unchecked.defaultof<_>)
                             else
                                 fail errNotImplicit)
                        ]
                        "Implicit check"
                )
                |> opt

            match isImplicitClass with
            | ValueSome _ ->
                // Implicit class: boundary is determined by indentation (offside rule).
                let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin
                let! body = withContext OffsideContext.Type ClassTypeBody.parseOffside
                let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
                return TypeDefn.Anon(typeName, primaryConstr, asDefn, equals, beginTok, body, endTok)
            | ValueNone ->
                // Type abbreviation: `type Name = [access] TargetType [with type-extensions]`
                let! representationAccess = pRepresentationAccess
                let! anchor = peekNextSyntaxToken

                let! t =
                    parseAbbrevRhs
                    |> recoverWith
                        StoppingTokens.afterTypeDefn
                        DiagnosticCode.MissingType
                        (missingOrSkipped Type<SyntaxToken>.Missing Type<SyntaxToken>.SkipsTokens)

                let! ext = TypeExtensionElements.parseOptAt anchor

                return TypeDefn.Abbrev(typeName, equals, representationAccess, t, ext)
        }

    /// Parses a type definition body after consuming the leading `type` or `and` keyword.
    let private parseBody (attrs: Attributes<SyntaxToken> voption) : Parser<TypeDefn<SyntaxToken>, _, _, _> =
        parser {

            let! typeName = TypeName.parseWithAttrs attrs

            // 1. Primary constructor and optional self-identifier (e.g., `type Foo(x) as this = ...`)
            let! primaryConstr = opt PrimaryConstrArgs.parse
            let! asDefn = opt AsDefn.parse

            // 2. Check for type extension (`with`), definition (`=`), or opaque/abstract type (neither).
            let! next2 = opt peekNextSyntaxToken
            let nextTok = next2 |> ValueOption.map (fun t -> t.Token)

            if nextTok = ValueSome Token.KWWith && primaryConstr.IsNone then
                let! elements = TypeExtensionElements.parse
                return TypeDefn.TypeExtension(typeName, elements)
            elif nextTok <> ValueSome Token.OpEquality then
                // Opaque/abstract declaration (e.g., `[<Measure>] type kg` or abstract types in signatures)
                return TypeDefn.AbstractType(typeName)
            else

                // 3. Consume '='
                let! equals = pEquals

                // 4. Branch based on type kind
                let! bodyStart = pBodyStart
                let next = bodyStart.Kind
                let bodyAnchor = bodyStart.Anchor

                match next.Token with
                | Token.KWStruct ->
                    // Explicit struct: `type S = struct ... end`
                    let! str = pStruct
                    let! body, endTok = StructTypeBody.parse pEnd
                    return TypeDefn.Struct(typeName, primaryConstr, asDefn, equals, str, body, endTok)

                | Token.KWInterface when primaryConstr.IsNone ->
                    // Lookahead disambiguation:
                    // - `interface Type with ...` -> Implicit class implementing an interface directly
                    // - `interface ... end`       -> Explicit interface definition
                    let! isInterfaceImpl =
                        lookAhead (
                            parser {
                                let! _ = pInterface
                                let! _ = Type.parse
                                let! w = peekNextSyntaxToken
                                return w.Token = Token.KWWith
                            }
                        )
                        |> opt
                        |>> ValueOption.defaultValue false

                    if isInterfaceImpl then
                        let! beginTok = nextSyntaxTokenVirtualIfNot Token.KWBegin
                        let! body = withContext OffsideContext.Type ClassTypeBody.parseOffside
                        let! endTok = nextSyntaxTokenVirtualIfNot Token.KWEnd
                        return TypeDefn.Anon(typeName, primaryConstr, asDefn, equals, beginTok, body, endTok)
                    else
                        let! intf = pInterface
                        let! body, endTok = InterfaceTypeBody.parse pEnd
                        return TypeDefn.Interface(typeName, equals, intf, body, endTok)

                | Token.KWClass ->
                    // Explicit class: `type C = class ... end`
                    let! cls = pClass
                    let! body, endTok = ClassTypeBody.parse pEnd
                    return TypeDefn.Class(typeName, primaryConstr, asDefn, equals, cls, body, endTok)

                | Token.KWDelegate ->
                    // Delegate: `type D = delegate of ...`
                    let! delSig = DelegateSig.parse
                    return TypeDefn.Delegate(typeName, equals, delSig)

                | Token.KWLBrace ->
                    // Record: `type R = [access] { field: int }`
                    // Note: SeqBlock prevents field type parsing from consuming the next field name.
                    let! representationAccess = pRepresentationAccess
                    let! lBrace = pLBrace
                    let! fields = many1 pRecordField
                    let! rBrace = pRBrace

                    let! ext = TypeExtensionElements.parseOptAt bodyAnchor

                    return TypeDefn.Record(typeName, equals, representationAccess, lBrace, fields, rBrace, ext)

                | Token.OpBar ->
                    // Leading `|`: Try enum first (`| Case = 1`), fallback to union (`| Case of int`).
                    // Note: SeqBlock aligned at `|` prevents case type suffixes from consuming
                    // subsequent top-level declarations on following lines.
                    let! representationAccess = pRepresentationAccess

                    return!
                        withContext
                            OffsideContext.SeqBlock
                            (choiceL
                                [
                                    parser {
                                        let! cases, bars = EnumTypeCases.parse
                                        return TypeDefn.Enum(typeName, equals, representationAccess, cases, bars)
                                    }
                                    parser {
                                        let! cases, bars = UnionTypeCases.parse
                                        let! ext = TypeExtensionElements.parseOptAt bodyAnchor
                                        return TypeDefn.Union(typeName, equals, representationAccess, cases, bars, ext)
                                    }
                                ]
                                "Union or Enum")

                | _ ->
                    // Handles unions without a leading pipe (e.g., `type U = A | B`).
                    // If it parses as a single parameterless case (`type T = Alias`),
                    // fail this branch and treat it as a type abbreviation instead.
                    return!
                        choiceL
                            [
                                parser {
                                    let! representationAccess = pRepresentationAccess
                                    let! cases, bars = UnionTypeCases.parse

                                    match cases.Length with
                                    | 1 when
                                        (match cases[0] with
                                         | UnionTypeCase.UnionTypeCase(_, UnionTypeCaseData.Nullary _) -> true
                                         | _ -> false)
                                        ->
                                        return! fail errSingleNullaryUnionCaseIsAbbrev
                                    | _ ->
                                        let! ext = TypeExtensionElements.parseOptAt bodyAnchor
                                        return TypeDefn.Union(typeName, equals, representationAccess, cases, bars, ext)
                                }
                                parseAbbrevOrImplicitClass typeName primaryConstr asDefn equals
                            ]
                            "Union or Type body"
        }

    /// Parses a primary type definition starting with 'type', returning the `type` keyword with it.
    let parse: Parser<SyntaxToken * TypeDefn<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! typeToken = pType
            // Attributes can appear before or after 'type' (e.g. `[<A>] type [<B>] T = ...`)
            let! attrsAfterType = opt Attributes.parse

            let mergedAttrs =
                match attrs, attrsAfterType with
                | ValueNone, ValueNone -> ValueNone
                | ValueSome a, ValueNone -> ValueSome a
                | ValueNone, ValueSome a -> ValueSome a
                | ValueSome a1, ValueSome a2 -> ValueSome(a1.AddRange(a2))

            let! defn = parseBody mergedAttrs
            return typeToken, defn
        }

    /// Parses a mutually recursive type definition continuation starting with 'and' (e.g., `and [<Attr>] T = ...`).
    let parseAndContinuation: Parser<TypeDefn<SyntaxToken>, _, _, _> =
        parser {
            let! _ = pAnd
            let! attrs = opt Attributes.parse
            return! parseBody attrs
        }

[<RequireQualifiedAccess>]
module ExceptionDefn =
    let parse: Parser<ExceptionDefn<SyntaxToken>, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = pException

            return!
                choiceL
                    [
                        // Full: exception Foo of int with members
                        parser {
                            let! anchor = peekNextSyntaxToken
                            let! caseData = UnionTypeCaseData.parseNary
                            let! ext = TypeExtensionElements.parseOptAt anchor
                            return ExceptionDefn.Full(attrs, exTok, caseData, ext)
                        }
                        // Abbreviation: exception Foo = Other.Exception
                        parser {
                            let! ident = pIdent
                            let! eq = pEquals
                            let! lid = LongIdent.parse
                            return ExceptionDefn.Abbreviation(attrs, exTok, ident, eq, lid)
                        }
                        // Nullary: exception Foo
                        parser {
                            let! anchor = peekNextSyntaxToken
                            let! ident = pIdent
                            let! ext = TypeExtensionElements.parseOptAt anchor

                            return
                                ExceptionDefn.Full(attrs, exTok, UnionTypeCaseData.Nullary(IdentOrOp.Ident ident), ext)
                        }
                    ]
                    "Exception definition"
        }
