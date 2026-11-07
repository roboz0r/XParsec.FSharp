namespace XParsec.FSharp.Lexer

open XParsec
open XParsec.Parsers


[<RequireQualifiedAccess>]
module Array =
    let inline tryFindIndexV ([<InlineIfLambda>] predicate: 'T -> bool) (array: 'T[]) =
        let rec loop i =
            if i >= array.Length then ValueNone
            else if predicate array[i] then ValueSome i
            else loop (i + 1)

        loop 0

[<AutoOpen>]
module MoreParsers =
    let skip (reader: Reader<'T, 'State, 'Input>) =
        reader.Skip()
        preturn () reader

    let skipN (n: int) (reader: Reader<'T, 'State, 'Input>) =
        reader.SkipN n
        preturn () reader
