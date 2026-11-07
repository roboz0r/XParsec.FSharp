# XParsec.FSharp

An F# lexer and parser built with [XParsec](https://github.com/roboz0r/XParsec).

This project was developed inside the XParsec repository and moved here with its history. It now uses XParsec as a NuGet package.

## Layout

- `src/XParsec.FSharp`: the lexer and parser
- `test/XParsec.FSharp.Tests`: Expecto tests, including the golden `.lexed`/`.parsed` snapshots in `data/`
- `bench/XParsec.FSharp.Benchmarks`: BenchmarkDotNet lexing and parsing benchmarks

## Build and test

```
dotnet build
dotnet test
```
