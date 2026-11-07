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

## Benchmarks

XParsec.FSharp compared with the F# compiler's parser (FCS 43.12.202) on three fixtures:

| Fixture | Source | Lines | Size |
|---|---|--:|--:|
| Small | XParsec `Types.fs` | 165 | 5 KB |
| Medium | XParsec `CharParsers.fs` | 1,054 | 41 KB |
| Large | FSharp.Core `prim-types.fs` | 7,355 | 446 KB |

### Lex and parse to a syntax tree

| Fixture | XParsec.FSharp | FCS | FCS ÷ XParsec | XParsec allocated | FCS allocated |
|---|--:|--:|--:|--:|--:|
| Small | 434 μs | 1,328 μs | 3.1× | 263 KB | 409 KB |
| Medium | 2.79 ms | 6.39 ms | 2.3× | 1,631 KB | 2,303 KB |
| Large | 45.9 ms | 129.2 ms | 2.8× | 24,717 KB | 47,902 KB |

### Lexing

| Fixture | XParsec.FSharp | FCS | FCS ÷ XParsec | XParsec allocated | FCS allocated |
|---|--:|--:|--:|--:|--:|
| Small | 74.7 μs | 843 μs | 11.3× | 38 KB | 195 KB |
| Medium | 495 μs | 4,524 μs | 9.2× | 262 KB | 1,162 KB |
| Large | 5.58 ms | 74.2 ms | 13.3× | 3,887 KB | 12,486 KB |

### Parsing from lexed tokens

FCS has no public entry point that parses a token stream, so this one is XParsec.FSharp only.

| Fixture | Time | Allocated |
|---|--:|--:|
| Small | 336 μs | 225 KB |
| Medium | 2.11 ms | 1,369 KB |
| Large | 42.8 ms | 20,830 KB |

Things to keep in mind when reading these:

- The two produce different trees. XParsec.FSharp builds a concrete syntax tree that keeps every
  token; FCS's `ParseFile` builds its untyped AST with ranges and trivia.
- FCS lexing is measured through `FSharpLexer.Tokenize`, its public tokenizer, which reports every
  token through a callback. FCS's parser uses its lexer internally, so the end-to-end table is the
  fairer comparison.
- FCS parses with `CompilingFSharpCore = true` so that it accepts `prim-types.fs`'s library-only
  syntax, and with its parse cache disabled.

Measured on 2026-11-07 at commit `0f3fa7d` with BenchmarkDotNet 0.15.8 and .NET 10.0.12 on an
AMD Ryzen 7 7800X3D running Windows 11. To run them yourself:

```
dotnet run -c Release --project bench/XParsec.FSharp.Benchmarks -- --filter '*'
```
