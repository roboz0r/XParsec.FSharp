namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.FSharp.Lexer


[<RequireQualifiedAccess>]
[<Struct>]
type TokenIndex =
    | Regular of int<token>
    | Virtual

[<Struct>]
type SyntaxToken =
    {
        PositionedToken: PositionedToken
        Index: TokenIndex
    }

    member this.StartIndex = this.PositionedToken.StartIndex
    member this.Token = this.PositionedToken.Token


/// Represents a source location or range within the token stream.
/// Used across parsing and diagnostics.
[<RequireQualifiedAccess>]
type Site =
    /// No source location (e.g. file-level errors or signature-level checks).
    | Nowhere
    /// A single token.
    | At of token: int<token>
    /// An inclusive range from `first` to `last` token.
    | Between of first: int<token> * last: int<token>
    /// A zero-width position immediately after `token`. Used for missing tokens or synthetic recovery points.
    | After of token: int<token>

[<RequireQualifiedAccess>]
module Site =

    /// Returns the `Site` for a token, or `Nowhere` if the token is virtual.
    let ofToken (tok: SyntaxToken) : Site =
        match tok.Index with
        | TokenIndex.Regular i -> Site.At i
        | TokenIndex.Virtual -> Site.Nowhere

    /// Returns a zero-width location (`Site.After`) immediately preceding `site`.
    /// Returns `At 0` if `site` is at the start of the file.
    let gapBefore (site: Site) : Site =
        match site with
        | Site.At i when i > 0<token> -> Site.After(i - 1<token>)
        | placed -> placed

    /// Returns the token's site, or `fallback` if the token is virtual.
    let ofTokenOr (fallback: Site) (tok: SyntaxToken) : Site =
        match ofToken tok with
        | Site.Nowhere -> fallback
        | positioned -> positioned

    /// Constructs a canonical token span. Ensures indices are ordered and collapses
    /// single-token ranges to `Site.At`.
    let between (first: int<token>) (last: int<token>) : Site =
        let lo = min first last
        let hi = max first last

        if lo = hi then Site.At lo else Site.Between(lo, hi)

    /// Normalizes a `Site` into its canonical form (ordering bounds and collapsing single-token ranges).
    let normalise (s: Site) : Site =
        match s with
        | Site.Between(first, last) -> between first last
        | Site.Nowhere
        | Site.At _
        | Site.After _ -> s

    /// Computes the bounding span covering all non-virtual tokens in `toks`.
    /// Returns `Nowhere` if all tokens are virtual.
    let spanning (toks: SyntaxToken seq) : Site =
        let mutable lo = ValueNone
        let mutable hi = ValueNone

        for tok in toks do
            match tok.Index with
            | TokenIndex.Regular i ->
                lo <-
                    ValueSome(
                        match lo with
                        | ValueSome l -> min l i
                        | ValueNone -> i
                    )

                hi <-
                    ValueSome(
                        match hi with
                        | ValueSome h -> max h i
                        | ValueNone -> i
                    )
            | TokenIndex.Virtual -> ()

        match lo, hi with
        | ValueSome l, ValueSome h -> between l h
        | _ -> Site.Nowhere

    /// Returns the span covering an entire long identifier (e.g. `A.B.C`).
    let ofLongIdent (li: LongIdent<SyntaxToken>) : Site = spanning li.Idents


/// Represents #if expressions used in conditional compilation
[<RequireQualifiedAccess>]
type IfExpr<'T> =
    | Term of 'T
    | And of IfExpr<'T> * 'T * IfExpr<'T> // &&
    | Or of IfExpr<'T> * 'T * IfExpr<'T> // ||
    | Not of 'T * IfExpr<'T> // !
    | Paren of 'T * IfExpr<'T> * 'T // ( ... )

// https://fsharp.github.io/fslang-spec/lexical-filtering/#1516-full-list-of-offside-contexts

[<RequireQualifiedAccess>]
type OffsideContext =
    | Let
    | If
    | Try
    | Lazy
    | Fun
    | Function
    /// The `with` keyword as part of a record expression or an object expression whose members use the syntax `{ new Foo with M() = 1 and N() = 2 }`
    | WithLet
    /// The `with` keyword as part of an extension, interface, or object expression whose members use the syntax `{ new Foo member x.M() = 1 member x. N() = 2 }`
    | WithAugment
    | Match
    /// Between the `for` and `do` or `->` in a `for` expression. The pattern and iterable must be indented
    /// past the `for` keyword, but the body can be at the same indent as `for` or indented further.
    /// The body is represented by a `Do` context, which is pushed after the `do` or `->`.
    | For
    | While
    | Then
    | Else
    | Do
    | Type
    | Namespace
    | Module
    /// The `member`, `abstract`, `default`, or `override` keyword, if the Member context is not already active, because multiple tokens may be present.
    /// - or -
    /// `(` is the next token after the `new` keyword. This distinguishes the member declaration `new(x) = ...` from the expression `new x()`
    | Member
    | Paren
    | Bracket
    | Brace
    | BracketBar
    | BraceBar
    | Begin
    | Struct
    | Sig
    | Quote
    /// The `with` keyword in a Try or Match context immediately after a `function` keyword.
    | MatchClauses
    /// An otherwise unprocessed keyword in a SeqBlock context.
    | Vanilla
    /// A sequence of items that must be column-aligned (the primary offside context).
    /// Pushed after `=` in Let/Member, after Then/Else/Try/Finally/Do contexts, after `->` in MatchClauses, etc.
    | SeqBlock

type Offside =
    {
        Context: OffsideContext
        Indent: int
        Token: PositionedToken
    }

[<RequireQualifiedAccess>]
module Offside =
    /// Determines whether this frame is a module-declaration block (either a nested `module X =`
    /// or the top-level file sequence block anchored on EOF) rather than an expression sequence.
    let isDeclBlock (frame: Offside) : bool =
        match frame.Context with
        | OffsideContext.Module -> true
        | OffsideContext.SeqBlock -> frame.Token.Token = Token.EOF
        | _ -> false

/// Identifies specific parse error conditions.
[<RequireQualifiedAccess>]
type DiagnosticCode =
    // TODO: Use F# error codes
    | Other of string
    /// A unit-of-measure constant cannot specify a type parameter.
    | TyparInConstant
    // Recovery-specific:
    | MissingExpression
    | MissingPattern
    | MissingType
    | MissingRule
    | MissingTypeDefn
    | MissingModuleElem
    | UnexpectedTopLevel
    | ExpectedEnd
    | ExpectedRParen
    | ExpectedRBracket
    | ExpectedRArrayBracket
    | ExpectedRBraceBar
    | ExpectedQuotationTypedRight
    | ExpectedQuotationUntypedRight
    /// An expected closing delimiter was missing and synthetically inserted.
    | UnclosedDelimiter of opened: Token * openedAt: Site * expected: Token
    /// A closing delimiter was present but did not match the opening delimiter (e.g. `{| … }`).
    | MismatchedDelimiter of opened: Token * openedAt: Site * expected: Token
    /// Input ends inside a block comment. Reported at the outermost `(*`.
    | UnterminatedComment of ending: CommentEnding

[<RequireQualifiedAccess>]
type Syntax =
    | Light
    | Verbose

/// Callback interface for structured parser trace events.
/// Null by default (`ParseState.Trace = null`), skipping tracing without overhead.
[<AllowNullLiteral>]
type TraceCallback() =
    abstract ContextPush: context: OffsideContext * indent: int * token: PositionedToken * stackDepth: int -> unit

    default _.ContextPush(_, _, _, _) = ()

    abstract ContextPop: context: OffsideContext * stackDepth: int -> unit
    default _.ContextPop(_, _) = ()

    abstract TokenConsumed: token: PositionedToken * index: int * col: int -> unit
    default _.TokenConsumed(_, _, _) = ()

    abstract TokenPeeked: token: PositionedToken * index: int * col: int -> unit
    default _.TokenPeeked(_, _, _) = ()

    abstract VirtualToken: token: Token * atStartIndex: int -> unit
    default _.VirtualToken(_, _) = ()

    abstract OffsideOk: token: PositionedToken * tokenCol: int * contextIndent: int * context: OffsideContext -> unit

    default _.OffsideOk(_, _, _, _) = ()

    abstract OffsideFail: token: PositionedToken * tokenCol: int * contextIndent: int * context: OffsideContext -> unit

    default _.OffsideFail(_, _, _, _) = ()

    abstract PermittedUndentation: token: PositionedToken * tokenCol: int * contextIndent: int * rule: string -> unit

    default _.PermittedUndentation(_, _, _, _) = ()

    abstract DiagnosticEmitted: code: DiagnosticCode * token: SyntaxToken -> unit

    default _.DiagnosticEmitted(_, _) = ()

    abstract SplitRAttrBracketSet: atStartIndex: int -> unit
    default _.SplitRAttrBracketSet(_) = ()

    abstract SplitRAttrBracketConsumed: atStartIndex: int -> unit
    default _.SplitRAttrBracketConsumed(_) = ()

    abstract SplitPowerMinusSet: atStartIndex: int -> unit
    default _.SplitPowerMinusSet(_) = ()

    abstract SplitPowerMinusConsumed: atStartIndex: int -> unit
    default _.SplitPowerMinusConsumed(_) = ()

/// A warning code written in a `#nowarn` or `#warnon` directive.
[<RequireQualifiedAccess>]
type WarnCode =
    /// An F# compiler code, written `42`, `"42"` or `"FS0042"`.
    | FSharp of number: int
    /// Any other code, such as `"V278"`.
    | Named of code: string

    override this.ToString() =
        match this with
        | FSharp n -> string n
        | Named code -> code

[<RequireQualifiedAccess>]
module WarnCode =

    /// The code written as the string `text` in a directive; `ValueNone` for empty text or text holding whitespace.
    let ofText (text: string) : WarnCode voption =
        let number (s: string) : int voption =
            match s.Length > 0 && Seq.forall System.Char.IsAsciiDigit s, System.Int32.TryParse s with
            | true, (true, n) -> ValueSome n
            | _ -> ValueNone

        let unprefixed =
            if text.StartsWith("FS", System.StringComparison.Ordinal) then
                text.Substring 2
            else
                text

        match number unprefixed with
        | ValueSome n -> ValueSome(WarnCode.FSharp n)
        | ValueNone when text.Length > 0 && not (Seq.exists System.Char.IsWhiteSpace text) ->
            ValueSome(WarnCode.Named text)
        | ValueNone -> ValueNone

[<Struct>]
type WarnDirective =
    {
        /// The zero-based line of the directive.
        Line: int<line>
        Code: WarnCode
        Suppress: bool
    }

/// Represents a syntax error encountered during parsing.
type Diagnostic =
    {
        Code: DiagnosticCode
        Token: SyntaxToken
        TokenEnd: SyntaxToken option
        Error: ParseError<PositionedToken, ParseState> option
    }

and [<ReferenceEquality; NoComparison>] ParseState =
    {
        Lexed: Lexed
        Context: Offside list
        Diagnostics: Diagnostic list
        ActiveDefines: ActiveDefines
        IndentationMode: Syntax
        mutable LastLine: int<line> // ok to be mutable since it's only used as a guess
        // ReprocessOpAfterTypeDeclaration: bool
        /// Number of characters consumed since the last type parameter. Used to
        /// disambiguate multi-character operators (like `>.` or `>>=`) when closing type arguments.
        CharsConsumedAfterTypeParams: int
        /// When true, the next `KWRAttrBracket` (`>]`) is split and treated as `KWRBracket` (`]`).
        SplitRAttrBracket: bool
        /// When true, a custom operator starting with `^-` is split into `^` and `-`.
        SplitPowerMinus: bool
        /// Accumulated #nowarn / #warnon directives (most recent first).
        WarnDirectives: WarnDirective list
        /// Callback for structured parse tracing (`null` when tracing is disabled).
        Trace: TraceCallback
    }

/// The concrete Readable slice type the F# parser reads from.
/// Future input-representation swaps should only change this alias.
type FSReadable = ReadableArray<PositionedToken>

/// An F# parser: produces 'T from the token stream.
type FSParser<'T> = Parser<'T, PositionedToken, ParseState, FSReadable>

/// An F# reader over the token stream.
type FSReader = Reader<PositionedToken, ParseState, FSReadable>


module DiagnosticCode =

    /// Returns the text representation of a token for diagnostic messages.
    let private spelling (t: Token) =
        match TokenInfo.withoutFlags t with
        | Token.KWLParen -> "("
        | Token.KWRParen -> ")"
        | Token.KWLBracket -> "["
        | Token.KWRBracket -> "]"
        | Token.KWLArrayBracket -> "[|"
        | Token.KWRArrayBracket -> "|]"
        | Token.KWLBrace -> "{"
        | Token.KWRBrace -> "}"
        | Token.KWLBraceBar -> "{|"
        | Token.KWRBraceBar -> "|}"
        | Token.KWLAttrBracket -> "[<"
        | Token.KWRAttrBracket -> ">]"
        | Token.OpQuotationTypedLeft -> "<@"
        | Token.OpQuotationTypedRight -> "@>"
        | Token.OpQuotationUntypedLeft -> "<@@"
        | Token.OpQuotationUntypedRight -> "@@>"
        | Token.KWEnd -> "end"
        | other -> string other

    /// Formats an "Expected '<token>'" error message.
    let expecting (t: Token) = $"Expected '{spelling t}'"

    /// Returns the error code identifier for a diagnostic.
    let code (c: DiagnosticCode) : string =
        match c with
        | DiagnosticCode.Other _ -> "Other"
        | DiagnosticCode.TyparInConstant -> "TyparInConstant"
        | DiagnosticCode.MissingExpression -> "MissingExpression"
        | DiagnosticCode.MissingPattern -> "MissingPattern"
        | DiagnosticCode.MissingType -> "MissingType"
        | DiagnosticCode.MissingRule -> "MissingRule"
        | DiagnosticCode.MissingTypeDefn -> "MissingTypeDefn"
        | DiagnosticCode.MissingModuleElem -> "MissingModuleElem"
        | DiagnosticCode.UnexpectedTopLevel -> "UnexpectedTopLevel"
        | DiagnosticCode.ExpectedEnd -> "ExpectedEnd"
        | DiagnosticCode.ExpectedRParen -> "ExpectedRParen"
        | DiagnosticCode.ExpectedRBracket -> "ExpectedRBracket"
        | DiagnosticCode.ExpectedRArrayBracket -> "ExpectedRArrayBracket"
        | DiagnosticCode.ExpectedRBraceBar -> "ExpectedRBraceBar"
        | DiagnosticCode.ExpectedQuotationTypedRight -> "ExpectedQuotationTypedRight"
        | DiagnosticCode.ExpectedQuotationUntypedRight -> "ExpectedQuotationUntypedRight"
        | DiagnosticCode.UnclosedDelimiter _ -> "UnclosedDelimiter"
        | DiagnosticCode.MismatchedDelimiter _ -> "MismatchedDelimiter"
        | DiagnosticCode.UnterminatedComment _ -> "UnterminatedComment"

    /// Formats the human-readable message for a diagnostic.
    let message (c: DiagnosticCode) : string =
        match c with
        | DiagnosticCode.Other msg -> msg
        | DiagnosticCode.TyparInConstant -> "A unit-of-measure on a constant cannot mention a type parameter"
        | DiagnosticCode.MissingExpression -> "Expected an expression"
        | DiagnosticCode.MissingPattern -> "Expected a pattern"
        | DiagnosticCode.MissingType -> "Expected a type"
        | DiagnosticCode.MissingRule -> "Expected a match rule"
        | DiagnosticCode.MissingTypeDefn -> "Expected a type definition"
        | DiagnosticCode.MissingModuleElem -> "Expected a module declaration"
        | DiagnosticCode.UnexpectedTopLevel -> "Unexpected token(s) at the top level"
        | DiagnosticCode.ExpectedEnd -> expecting Token.KWEnd
        | DiagnosticCode.ExpectedRParen -> expecting Token.KWRParen
        | DiagnosticCode.ExpectedRBracket -> expecting Token.KWRBracket
        | DiagnosticCode.ExpectedRArrayBracket -> expecting Token.KWRArrayBracket
        | DiagnosticCode.ExpectedRBraceBar -> expecting Token.KWRBraceBar
        | DiagnosticCode.ExpectedQuotationTypedRight -> expecting Token.OpQuotationTypedRight
        | DiagnosticCode.ExpectedQuotationUntypedRight -> expecting Token.OpQuotationUntypedRight
        | DiagnosticCode.UnclosedDelimiter(opened = opened; expected = expected) ->
            $"Unclosed '{spelling opened}': {expecting expected}"
        | DiagnosticCode.MismatchedDelimiter(opened = opened; expected = expected) ->
            $"Wrong close for '{spelling opened}': {expecting expected}"
        | DiagnosticCode.UnterminatedComment CommentEnding.Text -> "End of file in comment begun at or before here"
        | DiagnosticCode.UnterminatedComment CommentEnding.String ->
            "End of file in string embedded in comment begun at or before here"
        | DiagnosticCode.UnterminatedComment CommentEnding.VerbatimString ->
            "End of file in verbatim string embedded in comment begun at or before here"
        | DiagnosticCode.UnterminatedComment CommentEnding.TripleQuoteString ->
            "End of file in triple-quote string embedded in comment begun at or before here"

    /// Secondary label used for pointing back to the opening delimiter.
    let openedHereLabel = "unclosed delimiter"

module SyntaxToken =

    /// Returns the text of the token from source, unwrapping backtick-quoted identifiers.
    /// Returns an empty string for virtual tokens.
    let nameIn (lexed: Lexed) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular i -> lexed.GetTokenName i
        | TokenIndex.Virtual -> ""

    let syntaxToken token (index: int) =
        {
            PositionedToken = token
            Index = TokenIndex.Regular(index * 1<token>)
        }

    let virtualToken token =
        {
            PositionedToken = token
            Index = TokenIndex.Virtual
        }

    /// Synthetic token used when a diagnostic occurs at an offside position or past EOF.
    let nowhere = virtualToken (PositionedToken.Create(Token.EOF, 0))

module ParseState =
    /// Runs `action` on the trace callback if tracing is enabled.
    let inline ifTrace (state: ParseState) ([<InlineIfLambda>] action: TraceCallback -> unit) =
        if not (isNull state.Trace) then
            action state.Trace

    let createWithTracing (input: ParseInput) (trace: TraceCallback) =
        {
            Lexed = input.Lexed
            Context = []
            Diagnostics = []
            ActiveDefines = input.ActiveDefines
            IndentationMode = Syntax.Light
            LastLine = 0<line>
            // ReprocessOpAfterTypeDeclaration = false
            CharsConsumedAfterTypeParams = 0
            SplitRAttrBracket = false
            SplitPowerMinus = false
            WarnDirectives = []
            Trace = trace
        }

    let create (input: ParseInput) =
        {
            Lexed = input.Lexed
            Context = []
            Diagnostics = []
            ActiveDefines = input.ActiveDefines
            IndentationMode = Syntax.Light
            LastLine = 0<line>
            CharsConsumedAfterTypeParams = 0
            SplitRAttrBracket = false
            SplitPowerMinus = false
            WarnDirectives = []
            Trace = null
        }

    let pushOffside offsideCtx (state: ParseState) =
        let newState =
            { state with
                Context = offsideCtx :: state.Context
            }

        let stackDepth = newState.Context.Length

        ifTrace newState (fun t -> t.ContextPush(offsideCtx.Context, offsideCtx.Indent, offsideCtx.Token, stackDepth))

        newState

    let popOffside current (state: ParseState) =
        match state.Context with
        | [] -> invalidOp "Attempted to pop empty context"
        | top :: tail ->
            if top <> current then
                invalidOp $"Attempted to pop context {current} but top of stack was {top}"

            ifTrace state (fun t -> t.ContextPop(top.Context, state.Context.Length))
            { state with Context = tail }

    let addDiagnostic code startToken endToken error (state: ParseState) =
        ifTrace state (fun t -> t.DiagnosticEmitted(code, startToken))

        let diag =
            {
                Code = code
                Token = startToken
                TokenEnd = endToken
                Error = error
            }

        { state with
            Diagnostics = diag :: state.Diagnostics
        }

    /// Emits a single-token diagnostic without an end token or underlying parse error.
    let addDiagnosticAt code startToken state =
        addDiagnostic code startToken None None state

    /// Emits a single-token diagnostic that wraps an underlying parser error.
    let addDiagnosticWithError code startToken err state =
        addDiagnostic code startToken None (Some err) state

    let private findLineNumberImpl (lexed: Lexed) (guess: int<line>) (index: int<token>) =
        if index < 0<token> || index >= lexed.Tokens.LengthM then
            invalidArg (nameof index) "Index out of range"

        let lineStarts = lexed.LineStarts
        let lineCount = lineStarts.LengthM

        // Binary search: returns the largest line index where lineStarts[line] <= index.
        let rec search (low: int<line>) (high: int<line>) =
            if low >= high then
                low
            else
                let mid = low + (high - low + 1<line>) / 2

                if lineStarts.[mid] <= index then
                    search mid high
                else
                    search low (mid - 1<line>)

        // Narrow the search range using the cached line guess.
        let low, high =
            if guess >= 0<line> && guess < lineCount then
                if
                    lineStarts.[guess] <= index
                    && (guess = lineCount - 1<line> || lineStarts.[guess + 1<line>] > index)
                then
                    (guess, guess)
                else if lineStarts.[guess] < index then
                    (guess, lineCount - 1<line>)
                else
                    (0<line>, guess)
            else
                (0<line>, lineCount - 1<line>)

        search low high

    let findLineNumber (state: ParseState) (index: int<token>) =
        let lineNo = findLineNumberImpl state.Lexed state.LastLine index
        state.LastLine <- lineNo
        lineNo

    let rec private getIndentViaLineStarts (state: ParseState) (index: int<token>) =
        let currentLineTokenIndex = state.Lexed.LineStarts[state.LastLine]

        if index = currentLineTokenIndex then
            0
        elif index > currentLineTokenIndex then
            let nextLine = state.LastLine + 1<_>

            if nextLine < state.Lexed.LineStarts.LengthM then
                let nextLineTokenIndex = state.Lexed.LineStarts[nextLine]

                if index < nextLineTokenIndex then
                    let token = state.Lexed.Tokens[index]
                    let lineStartToken = state.Lexed.Tokens[currentLineTokenIndex]
                    token.StartIndex - lineStartToken.StartIndex
                else
                    state.LastLine <- findLineNumberImpl state.Lexed nextLine index
                    getIndentViaLineStarts state index
            else
                // Last line
                let token = state.Lexed.Tokens[index]
                token.StartIndex - state.Lexed.Tokens[currentLineTokenIndex].StartIndex
        else
            state.LastLine <- findLineNumberImpl state.Lexed (state.LastLine - 1<_>) index
            getIndentViaLineStarts state index

    /// Returns the indentation column of the token at `index`.
    let getIndent (state: ParseState) (index: int<token>) =
        let indent = state.Lexed.Indents[index]

        match indent with
        | Indents.UseLineStarts -> getIndentViaLineStarts state index
        | _ -> int indent

    let isTriviaToken (state: ParseState) (token: PositionedToken) =
        if token.InComment then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.LineComment
            | Token.Indent
            | Token.Whitespace
            | Token.BlockCommentStart
            | Token.BlockCommentEnd
            | Token.Newline -> true
            | Token.Tab -> state.IndentationMode = Syntax.Verbose
            | _ -> false

    let tokenString (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> ""
        | TokenIndex.Regular iT -> state.Lexed.GetTokenString(iT)

    let tokenStringIs (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let span = state.Lexed.GetTokenSpan(iT)
            span.SequenceEqual(s.AsSpan())

    let tokenStringStartsWith (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let span = state.Lexed.GetTokenSpan(iT)
            span.StartsWith(s.AsSpan())

    let isDefined (state: ParseState) (symbolToken: SyntaxToken) =
        let symbol = tokenString symbolToken state
        state.ActiveDefines.Contains(symbol)

/// Formats trace events as text lines and writes them to a `TextWriter`.
/// Can be subclassed to intercept or redirect trace output.
type WriterTraceCallback(lexed: Lexed, writer: System.IO.TextWriter) =
    inherit TraceCallback()

    /// Emits a formatted trace line (default writes to `writer`).
    abstract Write: line: string -> unit
    default _.Write(line) = writer.WriteLine(line)

    override this.ContextPush(ctx, indent, token, depth) =
        this.Write($"PUSH {ctx} indent={indent} token={token.Token} @{token.StartIndex} depth={depth}")

    override this.ContextPop(ctx, depth) = this.Write($"POP {ctx} depth={depth}")

    override this.TokenConsumed(token, index, col) =
        let line = lexed.GetLineForToken(index * 1<token>)
        this.Write($"CONSUME {token.Token} @{token.StartIndex} index={index} col={col} line={line}")

    override this.TokenPeeked(token, index, col) =
        let line = lexed.GetLineForToken(index * 1<token>)
        this.Write($"PEEK {token.Token} @{token.StartIndex} index={index} col={col} line={line}")

    override this.VirtualToken(token, startIndex) =
        this.Write($"VIRTUAL {token} @{startIndex}")

    override this.OffsideOk(token, tokenCol, contextIndent, ctx) =
        this.Write($"OFFSIDE_OK {token.Token} col={tokenCol} >= indent={contextIndent} ctx={ctx}")

    override this.OffsideFail(token, tokenCol, contextIndent, ctx) =
        this.Write($"OFFSIDE_FAIL {token.Token} col={tokenCol} < indent={contextIndent} ctx={ctx}")

    override this.PermittedUndentation(token, tokenCol, contextIndent, rule) =
        this.Write($"UNDENT_OK {token.Token} col={tokenCol} < indent={contextIndent} rule={rule}")

    override this.DiagnosticEmitted(code, token) =
        this.Write($"DIAGNOSTIC {code} @{token.StartIndex}")

    override this.SplitRAttrBracketSet(startIndex) =
        this.Write($"SPLIT_RATTR_SET @{startIndex}")

    override this.SplitRAttrBracketConsumed(startIndex) =
        this.Write($"SPLIT_RATTR_CONSUMED @{startIndex}")

    override this.SplitPowerMinusSet(startIndex) =
        this.Write($"SPLIT_POW_MINUS_SET @{startIndex}")

    override this.SplitPowerMinusConsumed(startIndex) =
        this.Write($"SPLIT_POW_MINUS_CONSUMED @{startIndex}")

[<RequireQualifiedAccess>]
module Reader =
    let ofParseInput (input: ParseInput) : Reader<_, ParseState, _> =
        let initialState = ParseState.create input
        Reader((input.Lexed.Tokens.AsReadableArray()), initialState, 0)
