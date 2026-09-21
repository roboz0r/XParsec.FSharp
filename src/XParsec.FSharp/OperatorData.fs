namespace XParsec.FSharp

open System

/// Defines the mapping between F# operator symbols and their compiled IL member names (e.g., `+` <-> `op_Addition`).
module OperatorData =

    [<Literal>]
    let OpNil = "op_Nil"

    [<Literal>]
    let OpColonColon = "op_ColonColon"

    [<Literal>]
    let OpAddition = "op_Addition"

    [<Literal>]
    let OpSubtraction = "op_Subtraction"

    [<Literal>]
    let OpMultiply = "op_Multiply"

    [<Literal>]
    let OpDivision = "op_Division"

    [<Literal>]
    let OpExponentiation = "op_Exponentiation"

    [<Literal>]
    let OpAppend = "op_Append"

    [<Literal>]
    let OpConcatenate = "op_Concatenate"

    [<Literal>]
    let OpModulus = "op_Modulus"

    [<Literal>]
    let OpBitwiseAnd = "op_BitwiseAnd"

    [<Literal>]
    let OpBitwiseOr = "op_BitwiseOr"

    [<Literal>]
    let OpExclusiveOr = "op_ExclusiveOr"

    [<Literal>]
    let OpLeftShift = "op_LeftShift"

    [<Literal>]
    let OpLogicalNot = "op_LogicalNot"

    [<Literal>]
    let OpRightShift = "op_RightShift"

    [<Literal>]
    let OpUnaryPlus = "op_UnaryPlus"

    [<Literal>]
    let OpUnaryNegation = "op_UnaryNegation"

    [<Literal>]
    let OpEquality = "op_Equality"

    [<Literal>]
    let OpInequality = "op_Inequality"

    [<Literal>]
    let OpLessThanOrEqual = "op_LessThanOrEqual"

    [<Literal>]
    let OpGreaterThanOrEqual = "op_GreaterThanOrEqual"

    [<Literal>]
    let OpLessThan = "op_LessThan"

    [<Literal>]
    let OpGreaterThan = "op_GreaterThan"

    [<Literal>]
    let OpDynamic = "op_Dynamic"

    [<Literal>]
    let OpDynamicAssignment = "op_DynamicAssignment"

    [<Literal>]
    let OpPipeRight = "op_PipeRight"

    [<Literal>]
    let OpPipeRight2 = "op_PipeRight2"

    [<Literal>]
    let OpPipeRight3 = "op_PipeRight3"

    [<Literal>]
    let OpPipeLeft = "op_PipeLeft"

    [<Literal>]
    let OpPipeLeft2 = "op_PipeLeft2"

    [<Literal>]
    let OpPipeLeft3 = "op_PipeLeft3"

    [<Literal>]
    let OpDereference = "op_Dereference"

    [<Literal>]
    let OpComposeRight = "op_ComposeRight"

    [<Literal>]
    let OpComposeLeft = "op_ComposeLeft"

    [<Literal>]
    let OpQuotation = "op_Quotation"

    [<Literal>]
    let OpQuotationUntyped = "op_QuotationUntyped"

    [<Literal>]
    let OpSplice = "op_Splice"

    [<Literal>]
    let OpSpliceUntyped = "op_SpliceUntyped"

    [<Literal>]
    let OpAddressOf = "op_AddressOf"

    [<Literal>]
    let OpIntegerAddressOf = "op_IntegerAddressOf"

    [<Literal>]
    let OpBooleanOr = "op_BooleanOr"

    [<Literal>]
    let OpBooleanAnd = "op_BooleanAnd"

    [<Literal>]
    let OpAdditionAssignment = "op_AdditionAssignment"

    [<Literal>]
    let OpSubtractionAssignment = "op_SubtractionAssignment"

    [<Literal>]
    let OpMultiplyAssignment = "op_MultiplyAssignment"

    [<Literal>]
    let OpDivisionAssignment = "op_DivisionAssignment"

    [<Literal>]
    let OpRange = "op_Range"

    [<Literal>]
    let OpRangeStep = "op_RangeStep"

    /// Compiled name for whitespace function application.
    [<Literal>]
    let OpSpace = "op_Space"

    /// Predefined F# operators and their standard compiled names.
    let private symbolNames: (string * string)[] =
        [|
            "[]", OpNil
            "::", OpColonColon
            "+", OpAddition
            "-", OpSubtraction
            "*", OpMultiply
            "/", OpDivision
            "**", OpExponentiation
            "@", OpAppend
            "^", OpConcatenate
            "%", OpModulus
            "&&&", OpBitwiseAnd
            "|||", OpBitwiseOr
            "^^^", OpExclusiveOr
            "<<<", OpLeftShift
            "~~~", OpLogicalNot
            ">>>", OpRightShift
            "~+", OpUnaryPlus
            "~-", OpUnaryNegation
            "=", OpEquality
            "<>", OpInequality
            "<=", OpLessThanOrEqual
            ">=", OpGreaterThanOrEqual
            "<", OpLessThan
            ">", OpGreaterThan
            "?", OpDynamic
            "?<-", OpDynamicAssignment
            "|>", OpPipeRight
            "||>", OpPipeRight2
            "|||>", OpPipeRight3
            "<|", OpPipeLeft
            "<||", OpPipeLeft2
            "<|||", OpPipeLeft3
            "!", OpDereference
            ">>", OpComposeRight
            "<<", OpComposeLeft
            "<@ @>", OpQuotation
            "<@@ @@>", OpQuotationUntyped
            "~%", OpSplice
            "~%%", OpSpliceUntyped
            "~&", OpAddressOf
            "~&&", OpIntegerAddressOf
            "||", OpBooleanOr
            "&&", OpBooleanAnd
            "+=", OpAdditionAssignment
            "-=", OpSubtractionAssignment
            "*=", OpMultiplyAssignment
            "/=", OpDivisionAssignment
            "..", OpRange
            ".. ..", OpRangeStep
        |]

    let private symbolOfCompiledName: Map<string, string> =
        symbolNames |> Seq.map (fun (symbol, compiled) -> compiled, symbol) |> Map.ofSeq

    /// Gets the original source symbol for a standard compiled operator name (e.g., `op_Addition` -> `+`).
    /// Returns `ValueNone` if the name is not in the predefined operator table.
    let sourceSpelling (compiledName: string) : string voption =
        match Map.tryFind compiledName symbolOfCompiledName with
        | Some symbol -> ValueSome symbol
        | None -> ValueNone

    /// Character-to-identifier mappings used to generate compiled names for custom operators.
    let private charNames: (char * string)[] =
        [|
            '>', "Greater"
            '<', "Less"
            '+', "Plus"
            '-', "Minus"
            '*', "Multiply"
            '=', "Equals"
            '~', "Twiddle"
            '%', "Percent"
            '.', "Dot"
            '$', "Dollar"
            '&', "Amp"
            '|', "Bar"
            '@', "At"
            '#', "Hash"
            '^', "Hat"
            '!', "Bang"
            '?', "Qmark"
            '/', "Divide"
            ':', "Colon"
            '(', "LParen"
            ',', "Comma"
            ')', "RParen"
            ' ', "Space"
            '[', "LBrack"
            ']', "RBrack"
        |]

    /// Returns the compiled name for an operator symbol.
    ///
    /// Uses standard predefined names when available (e.g., `+` -> `op_Addition`),
    /// or generates an encoded name for custom operators (e.g., `<+>` -> `op_LessPlusGreater`).
    let nameOfSymbol (s: string) =
        if String.IsNullOrEmpty s then
            invalidArg (nameof s) "Operator cannot be null or empty."
        elif String.IsNullOrWhiteSpace s then
            OpSpace
        else
            match Array.tryFind (fun (symbol, _) -> symbol = s) symbolNames with
            | Some(_, name) -> name
            | None ->
                let sb = System.Text.StringBuilder("op_")

                for c in s do
                    match Array.tryFind (fun (ch, _) -> ch = c) charNames with
                    | Some(_, spelling) -> sb.Append(spelling) |> ignore
                    | None -> invalidArg (nameof s) (sprintf "Operator %s contains invalid character '%c'." s c)

                sb.ToString()
