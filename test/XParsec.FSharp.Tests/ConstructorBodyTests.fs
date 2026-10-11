module XParsec.FSharp.Tests.ConstructorBodyTests

open System
open System.IO

open Expecto

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let private parse (source: string) : Lexed * FSharpAst<SyntaxToken> =
    let lexed = Lexing.lexString (source.Replace("\r\n", "\n"))
    let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

    match FSharpAst.parse reader with
    | Ok ast -> lexed, ast
    | Error e -> failtestf "Parsing failed:\n%s" (ErrorFormatting.splitAndFormatTokenErrors e)

/// The classified bodies of the secondary constructors declared in `ast`, in source order.
let private constructorBodies (ast: FSharpAst<SyntaxToken>) : ConstructorBody<SyntaxToken> list =
    let file =
        match ast with
        | FSharpAst.ImplementationFile file -> file
        | _ -> failtest "Expected an implementation file"

    [
        for elem in CstDeclarations.implFileElems file do
            match elem with
            | ModuleElem.Type(typeDefns = defns) ->
                for defn in defns do
                    match TypeDefnPatterns.tryClassLikeDecl defn with
                    | ValueSome decl ->
                        for element in decl.Body.elements do
                            match element with
                            | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(shape = shape)) -> yield shape
                            | _ -> ()
                    | ValueNone -> ()
            | _ -> ()
    ]

let private renderInit (init: ConstructorInit<SyntaxToken>) : string =
    match init with
    | ConstructorInit.Fields(inherits = ValueSome(ClassInheritsDecl(expr = ValueSome _))) -> "{inherit(arg)}"
    | ConstructorInit.Fields(inherits = ValueSome(ClassInheritsDecl(expr = ValueNone))) -> "{inherit}"
    | ConstructorInit.Fields(inherits = ValueNone) -> "{fields}"
    | ConstructorInit.Delegated _ -> "new"
    | ConstructorInit.Expression _ -> "expr"

let rec private render (body: ConstructorBody<SyntaxToken>) : string =
    match body with
    | ConstructorBody.Statement(rest = rest) -> $"stmt; {render rest}"
    | ConstructorBody.Postlude(body = inner) -> $"({render inner}) then _"
    | ConstructorBody.Let(isRec = ValueSome _; body = inner) -> $"let rec in {render inner}"
    | ConstructorBody.Let(body = inner) -> $"let in {render inner}"
    | ConstructorBody.Conditional(thenBranch = t; elseBranch = e) -> $"if ({render t}) else ({render e})"
    | ConstructorBody.Match(arms = arms) ->
        let rendered = arms |> Seq.map (fun arm -> render arm.Body)
        $"""match {String.Join(" | ", rendered)}"""
    | ConstructorBody.Init init -> renderInit init

let private fixtureBodies (fileName: string) =
    let source = File.ReadAllText(Path.Combine(testDataDir.Value, fileName))
    let _, ast = parse source
    constructorBodies ast

let private classifies (fileName: string) (expected: string list) =
    test fileName {
        let actual = fixtureBodies fileName |> List.map render
        Expect.equal actual expected "Constructor body shapes"
    }

[<Tests>]
let classifierTests =
    testList
        "ConstructorBody"
        [
            classifies
                "417_ctor_if_then_statement_before_init.fs"
                [
                    "stmt; let in let in let in {fields}"
                    "stmt; stmt; {fields}"
                    "stmt; {fields}"
                ]

            classifies
                "418_ctor_elif_chains.fs"
                [
                    "if ({fields}) else (if ({fields}) else ({fields}))"
                    "if ({fields}) else (if ({fields}) else (if ({fields}) else ({fields})))"
                    "if ({fields}) else (if ({fields}) else ({fields}))"
                ]

            classifies
                "419_ctor_inherit_nested_conditionals.fs"
                [
                    "if (new) else (if (new) else (if ({inherit(arg)}) else ({inherit(arg)})))"
                    "if (if ({inherit(arg)}) else (new)) else ({inherit(arg)})"
                ]

            classifies
                "420_ctor_statement_forms.fs"
                [ "stmt; stmt; {fields}"; "stmt; stmt; {fields}"; "stmt; {fields}" ]

            classifies
                "421_ctor_then_postludes.fs"
                [
                    "expr"
                    "if (({inherit(arg)}) then _) else ({inherit(arg)})"
                    "({inherit(arg)}) then _"
                    "({inherit(arg)}) then _"
                    "({inherit(arg)}) then _"
                    "stmt; ({inherit(arg)}) then _"
                    "(if ({inherit(arg)}) else ({inherit(arg)})) then _"
                    "let in ({inherit(arg)}) then _"
                ]

            classifies
                "422_ctor_match_and_delegation.fs"
                [
                    "expr"
                    "match {inherit(arg)} | new | expr"
                    "{inherit(arg)}"
                    "{inherit(arg)}"
                    "{inherit(arg)}"
                    "new"
                    "{inherit}"
                    "{inherit(arg)}"
                    "{inherit(arg)}"
                    "expr"
                ]

            classifies "423_record_inherit_and_then_outside_ctor.fs" [ "stmt; {inherit(arg)}" ]

            classifies "404_secondary_ctor_inherit_semicolon.fs" [ "{inherit(arg)}"; "{inherit}"; "{inherit}" ]

            classifies "254_constructor_then.fs" [ "(expr) then _" ]

            classifies "429_empty_braces.fs" [ "{fields}" ]

            classifies "430_ctor_then_postlude_annotation.fs" [ "(expr) then _" ]

            classifies "431_new_and_inherit_array_arguments.fs" [ "{inherit(arg)}" ]

            classifies "434_object_expression_arguments.fs" [ "expr"; "{inherit(arg)}" ]

            classifies "383_ctor_let_seq_then.fs" [ "let in stmt; (expr) then _" ]

            test "inits lists every initialisation in source order" {
                let inits =
                    fixtureBodies "419_ctor_inherit_nested_conditionals.fs"
                    |> List.map (ConstructorBody.inits >> Seq.map renderInit >> List.ofSeq)

                Expect.equal
                    inits
                    [
                        [ "new"; "new"; "{inherit(arg)}"; "{inherit(arg)}" ]
                        [ "{inherit(arg)}"; "new"; "{inherit(arg)}" ]
                    ]
                    "Initialisations"
            }

            test "a let rec group and a use binding" {
                let source =
                    """
module M

type R =
    val X: int

    new(n: int) =
        let rec f x = if x = 0 then 0 else g (x - 1)
        and g x = f x
        { X = f n }

    new(n: string) =
        use d = { new System.IDisposable with member _.Dispose() = () }
        { X = 0 }

    new(n: float) = if n = 0.0 then { X = 0 } elif n = 1.0 then { X = 1 }
"""

                let _, ast = parse source
                let actual = constructorBodies ast |> List.map render
                // `use` and an `if` without `else` are not constructor-body forms (FS0696, FS0001).
                Expect.equal actual [ "let rec in {fields}"; "expr"; "expr" ] "Constructor body shapes"
            }
        ]

/// Builds a constructor whose body nests `depth` conditionals through `shape`.
let private nestedConstructor (depth: int) (shape: int -> string -> string) =
    let header =
        """module M

type B(x: int) =
    member _.X = x

type D =
    inherit B
    val Y: int

    new(n: int) =
"""

    header + shape depth (String(' ', 8))

/// `if false then` nested without `else`, followed by an initialisation.
let private nestedWithoutElse (depth: int) (indent: string) =
    let lines =
        [
            for i in 0 .. depth - 1 -> indent + String(' ', 4 * i) + "if false then"
            yield indent + String(' ', 4 * depth) + "()"
            yield indent + "{ inherit B(n); Y = n }"
        ]

    String.Join("\n", lines) + "\n"

/// `if ... then new D(...) else` nested through indented `else` branches, ending in an
/// `inherit` initialisation.
let private nestedElse (depth: int) (indent: string) =
    let lines =
        [
            for i in 0 .. depth - 1 do
                let pad = indent + String(' ', 4 * i)
                yield $"{pad}if n = {i} then new D({i})"
                yield $"{pad}else"
            yield indent + String(' ', 4 * depth) + "{ inherit B(n); Y = n }"
        ]

    String.Join("\n", lines) + "\n"

/// The number of token consumptions per lexed token when parsing `source`.
let private consumptionsPerToken (source: string) : float =
    let lexed = Lexing.lexString source
    let mutable consumed = 0

    let trace =
        { new TraceCallback() with
            override _.TokenConsumed(_, _, _) = consumed <- consumed + 1
        }

    let reader = ofParseInputWithTracing (lexed.WithDefines Set.empty) trace

    match FSharpAst.parse reader with
    | Ok _ ->
        Expect.isEmpty reader.State.Diagnostics "Diagnostics"
        float consumed / float lexed.Tokens.Length
    | Error e -> failtestf "Parsing failed:\n%s" (ErrorFormatting.splitAndFormatTokenErrors e)

[<Tests>]
let scalingTests =
    // A reparse of each nested branch makes the ratio grow with depth; a single pass keeps it constant.
    let bound = 2.0

    testList
        "ConstructorBodyScaling"
        [
            test "nested conditionals without else parse in linear time" {
                let ratio = consumptionsPerToken (nestedConstructor 100 nestedWithoutElse)
                Expect.isLessThan ratio bound "Token consumptions per lexed token"
            }

            test "nested else branches ending in an inherit initialisation parse in linear time" {
                let ratio = consumptionsPerToken (nestedConstructor 100 nestedElse)
                Expect.isLessThan ratio bound "Token consumptions per lexed token"
            }
        ]
