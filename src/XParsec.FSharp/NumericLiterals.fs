namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open XParsec.FSharp

// Evaluates numeric literal tokens using the base and kind metadata provided by the lexer.

/// Supported primitive integer types.
[<RequireQualifiedAccess>]
type IntKind =
    | SByte
    | Byte
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    /// Pointer-sized integer (nativeint / unativeint). Not supported as a CLR enum underlying type.
    | NativeInt
    | UNativeInt

/// Metadata and helper functions for integer types.
module IntKind =

    let all: IntKind list =
        [
            IntKind.SByte
            IntKind.Byte
            IntKind.Int16
            IntKind.UInt16
            IntKind.Int32
            IntKind.UInt32
            IntKind.Int64
            IntKind.UInt64
            IntKind.NativeInt
            IntKind.UNativeInt
        ]

    /// Returns the F# literal suffix (e.g., "y" for 10y, "UL" for 10UL, "" for int32).
    let suffix (k: IntKind) : string =
        match k with
        | IntKind.SByte -> "y"
        | IntKind.Byte -> "uy"
        | IntKind.Int16 -> "s"
        | IntKind.UInt16 -> "us"
        | IntKind.Int32 -> ""
        | IntKind.UInt32 -> "u"
        | IntKind.Int64 -> "L"
        | IntKind.UInt64 -> "UL"
        | IntKind.NativeInt -> "n"
        | IntKind.UNativeInt -> "un"

    let isSigned (k: IntKind) : bool =
        match k with
        | IntKind.SByte
        | IntKind.Int16
        | IntKind.Int32
        | IntKind.Int64
        | IntKind.NativeInt -> true
        | IntKind.Byte
        | IntKind.UInt16
        | IntKind.UInt32
        | IntKind.UInt64
        | IntKind.UNativeInt -> false

    /// True if the type is pointer-sized (nativeint or unativeint).
    let isNative (k: IntKind) : bool =
        match k with
        | IntKind.NativeInt
        | IntKind.UNativeInt -> true
        | IntKind.SByte
        | IntKind.Byte
        | IntKind.Int16
        | IntKind.UInt16
        | IntKind.Int32
        | IntKind.UInt32
        | IntKind.Int64
        | IntKind.UInt64 -> false

    /// Maps a lexer NumericKind to an IntKind.
    /// Returns ValueNone for non-integers (floats, decimals, and custom literals).
    let ofNumericKind (kind: NumericKind) : IntKind voption =
        match kind with
        | NumericKind.SByte -> ValueSome IntKind.SByte
        | NumericKind.Byte -> ValueSome IntKind.Byte
        | NumericKind.Int16 -> ValueSome IntKind.Int16
        | NumericKind.UInt16 -> ValueSome IntKind.UInt16
        | NumericKind.Int32 -> ValueSome IntKind.Int32
        | NumericKind.UInt32 -> ValueSome IntKind.UInt32
        | NumericKind.Int64 -> ValueSome IntKind.Int64
        | NumericKind.UInt64 -> ValueSome IntKind.UInt64
        | NumericKind.NativeInt -> ValueSome IntKind.NativeInt
        | NumericKind.UNativeInt -> ValueSome IntKind.UNativeInt
        | _ -> ValueNone

[<RequireQualifiedAccess>]
type IntValue =
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | NativeInt of int64
    | UNativeInt of uint64

/// Bitwise operations (OR, AND, XOR).
[<RequireQualifiedAccess>]
type IntBitwiseOp =
    | Or
    | And
    | Xor

/// Arithmetic operations (+, -, *) with two's-complement wrapping.
[<RequireQualifiedAccess>]
type IntArithOp =
    | Add
    | Subtract
    | Multiply

/// Bit shift operations (<<<, >>>). Shift counts are always int32.
[<RequireQualifiedAccess>]
type IntShiftOp =
    | Left
    | Right

/// Reasons why compile-time constant folding failed.
[<RequireQualifiedAccess>]
type IntFoldRejection =
    /// Both operands must be the same integer type.
    | KindMismatch
    /// Cannot fold pointer-sized types (nativeint / unativeint) because their bit width is target-dependent.
    | TargetWidth
    /// Shift count is negative or exceeds the bit width of the type.
    | ShiftCount

module IntValue =

    let kind (v: IntValue) : IntKind =
        match v with
        | IntValue.SByte _ -> IntKind.SByte
        | IntValue.Byte _ -> IntKind.Byte
        | IntValue.Int16 _ -> IntKind.Int16
        | IntValue.UInt16 _ -> IntKind.UInt16
        | IntValue.Int32 _ -> IntKind.Int32
        | IntValue.UInt32 _ -> IntKind.UInt32
        | IntValue.Int64 _ -> IntKind.Int64
        | IntValue.UInt64 _ -> IntKind.UInt64
        | IntValue.NativeInt _ -> IntKind.NativeInt
        | IntValue.UNativeInt _ -> IntKind.UNativeInt

    /// Converts an int64 to the specified integer kind, truncating higher bits.
    let ofInt64 (k: IntKind) (n: int64) : IntValue =
        match k with
        | IntKind.SByte -> IntValue.SByte(sbyte n)
        | IntKind.Byte -> IntValue.Byte(byte n)
        | IntKind.Int16 -> IntValue.Int16(int16 n)
        | IntKind.UInt16 -> IntValue.UInt16(uint16 n)
        | IntKind.Int32 -> IntValue.Int32(int32 n)
        | IntKind.UInt32 -> IntValue.UInt32(uint32 n)
        | IntKind.Int64 -> IntValue.Int64 n
        | IntKind.UInt64 -> IntValue.UInt64(uint64 n)
        | IntKind.NativeInt -> IntValue.NativeInt n
        | IntKind.UNativeInt -> IntValue.UNativeInt(uint64 n)

    let zero (k: IntKind) : IntValue = ofInt64 k 0L

    /// Parses a string representation of an integer with the given radix.
    /// Throws OverflowException or FormatException on invalid values.
    let parse (k: IntKind) (digits: string) (radix: int) : IntValue =
        match k with
        | IntKind.SByte -> IntValue.SByte(Convert.ToSByte(digits, radix))
        | IntKind.Byte -> IntValue.Byte(Convert.ToByte(digits, radix))
        | IntKind.Int16 -> IntValue.Int16(Convert.ToInt16(digits, radix))
        | IntKind.UInt16 -> IntValue.UInt16(Convert.ToUInt16(digits, radix))
        | IntKind.Int32 -> IntValue.Int32(Convert.ToInt32(digits, radix))
        | IntKind.UInt32 -> IntValue.UInt32(Convert.ToUInt32(digits, radix))
        | IntKind.Int64 -> IntValue.Int64(Convert.ToInt64(digits, radix))
        | IntKind.UInt64 -> IntValue.UInt64(Convert.ToUInt64(digits, radix))
        | IntKind.NativeInt -> IntValue.NativeInt(Convert.ToInt64(digits, radix))
        | IntKind.UNativeInt -> IntValue.UNativeInt(Convert.ToUInt64(digits, radix))

    /// Formats the integer as a decimal string.
    let render (v: IntValue) : string =
        match v with
        | IntValue.SByte n -> string n
        | IntValue.Byte n -> string n
        | IntValue.Int16 n -> string n
        | IntValue.UInt16 n -> string n
        | IntValue.Int32 n -> string n
        | IntValue.UInt32 n -> string n
        | IntValue.Int64 n -> string n
        | IntValue.UInt64 n -> string n
        | IntValue.NativeInt n -> string n
        | IntValue.UNativeInt n -> string n

    /// Boxes the value as its corresponding .NET primitive (used for CLR enum metadata).
    let boxed (v: IntValue) : obj =
        match v with
        | IntValue.SByte n -> box n
        | IntValue.Byte n -> box n
        | IntValue.Int16 n -> box n
        | IntValue.UInt16 n -> box n
        | IntValue.Int32 n -> box n
        | IntValue.UInt32 n -> box n
        | IntValue.Int64 n -> box n
        | IntValue.UInt64 n -> box n
        | IntValue.NativeInt n -> box (nativeint n)
        | IntValue.UNativeInt n -> box (unativeint n)

    /// Negates a signed integer using two's-complement wrapping.
    /// Returns ValueNone for unsigned types.
    let negate (v: IntValue) : IntValue voption =
        match v with
        | IntValue.SByte n -> ValueSome(IntValue.SByte(-n))
        | IntValue.Int16 n -> ValueSome(IntValue.Int16(-n))
        | IntValue.Int32 n -> ValueSome(IntValue.Int32(-n))
        | IntValue.Int64 n -> ValueSome(IntValue.Int64(-n))
        | IntValue.NativeInt n -> ValueSome(IntValue.NativeInt(-n))
        | IntValue.Byte _
        | IntValue.UInt16 _
        | IntValue.UInt32 _
        | IntValue.UInt64 _
        | IntValue.UNativeInt _ -> ValueNone

    /// Evaluates a bitwise operation. Returns ValueNone if operand types do not match.
    let bitwise (op: IntBitwiseOp) (l: IntValue) (r: IntValue) : IntValue voption =
        let inline apply (a: ^a) (b: ^a) : ^a =
            match op with
            | IntBitwiseOp.Or -> a ||| b
            | IntBitwiseOp.And -> a &&& b
            | IntBitwiseOp.Xor -> a ^^^ b

        match l, r with
        | IntValue.SByte a, IntValue.SByte b -> ValueSome(IntValue.SByte(apply a b))
        | IntValue.Byte a, IntValue.Byte b -> ValueSome(IntValue.Byte(apply a b))
        | IntValue.Int16 a, IntValue.Int16 b -> ValueSome(IntValue.Int16(apply a b))
        | IntValue.UInt16 a, IntValue.UInt16 b -> ValueSome(IntValue.UInt16(apply a b))
        | IntValue.Int32 a, IntValue.Int32 b -> ValueSome(IntValue.Int32(apply a b))
        | IntValue.UInt32 a, IntValue.UInt32 b -> ValueSome(IntValue.UInt32(apply a b))
        | IntValue.Int64 a, IntValue.Int64 b -> ValueSome(IntValue.Int64(apply a b))
        | IntValue.UInt64 a, IntValue.UInt64 b -> ValueSome(IntValue.UInt64(apply a b))
        | IntValue.NativeInt a, IntValue.NativeInt b -> ValueSome(IntValue.NativeInt(apply a b))
        | IntValue.UNativeInt a, IntValue.UNativeInt b -> ValueSome(IntValue.UNativeInt(apply a b))
        | _ -> ValueNone

    /// Evaluates wrapping arithmetic (+, -, *).
    /// Fails if operand types mismatch or if folding target-dependent pointer types.
    let arithmetic (op: IntArithOp) (l: IntValue) (r: IntValue) : Result<IntValue, IntFoldRejection> =
        let inline apply (a: ^a) (b: ^a) : ^a =
            match op with
            | IntArithOp.Add -> a + b
            | IntArithOp.Subtract -> a - b
            | IntArithOp.Multiply -> a * b

        match l, r with
        | IntValue.SByte a, IntValue.SByte b -> Ok(IntValue.SByte(apply a b))
        | IntValue.Byte a, IntValue.Byte b -> Ok(IntValue.Byte(apply a b))
        | IntValue.Int16 a, IntValue.Int16 b -> Ok(IntValue.Int16(apply a b))
        | IntValue.UInt16 a, IntValue.UInt16 b -> Ok(IntValue.UInt16(apply a b))
        | IntValue.Int32 a, IntValue.Int32 b -> Ok(IntValue.Int32(apply a b))
        | IntValue.UInt32 a, IntValue.UInt32 b -> Ok(IntValue.UInt32(apply a b))
        | IntValue.Int64 a, IntValue.Int64 b -> Ok(IntValue.Int64(apply a b))
        | IntValue.UInt64 a, IntValue.UInt64 b -> Ok(IntValue.UInt64(apply a b))
        | IntValue.NativeInt _, IntValue.NativeInt _
        | IntValue.UNativeInt _, IntValue.UNativeInt _ -> Error IntFoldRejection.TargetWidth
        | _ -> Error IntFoldRejection.KindMismatch

    /// Evaluates a bit shift (arithmetic for signed, logical for unsigned).
    /// Fails if the shift count is out of range or on pointer-sized types.
    let shift (op: IntShiftOp) (v: IntValue) (count: int) : Result<IntValue, IntFoldRejection> =
        let inline at (width: int) (mk: ^a -> IntValue) (a: ^a) : Result<IntValue, IntFoldRejection> =
            if count < 0 || count >= width then
                Error IntFoldRejection.ShiftCount
            else
                match op with
                | IntShiftOp.Left -> Ok(mk (a <<< count))
                | IntShiftOp.Right -> Ok(mk (a >>> count))

        match v with
        | IntValue.SByte a -> at 8 IntValue.SByte a
        | IntValue.Byte a -> at 8 IntValue.Byte a
        | IntValue.Int16 a -> at 16 IntValue.Int16 a
        | IntValue.UInt16 a -> at 16 IntValue.UInt16 a
        | IntValue.Int32 a -> at 32 IntValue.Int32 a
        | IntValue.UInt32 a -> at 32 IntValue.UInt32 a
        | IntValue.Int64 a -> at 64 IntValue.Int64 a
        | IntValue.UInt64 a -> at 64 IntValue.UInt64 a
        | IntValue.NativeInt _
        | IntValue.UNativeInt _ -> Error IntFoldRejection.TargetWidth

    /// Evaluates bitwise NOT (~~~). Inverts all bits for the type's width.
    let complement (v: IntValue) : Result<IntValue, IntFoldRejection> =
        match v with
        | IntValue.SByte a -> Ok(IntValue.SByte(~~~a))
        | IntValue.Byte a -> Ok(IntValue.Byte(~~~a))
        | IntValue.Int16 a -> Ok(IntValue.Int16(~~~a))
        | IntValue.UInt16 a -> Ok(IntValue.UInt16(~~~a))
        | IntValue.Int32 a -> Ok(IntValue.Int32(~~~a))
        | IntValue.UInt32 a -> Ok(IntValue.UInt32(~~~a))
        | IntValue.Int64 a -> Ok(IntValue.Int64(~~~a))
        | IntValue.UInt64 a -> Ok(IntValue.UInt64(~~~a))
        | IntValue.NativeInt _
        | IntValue.UNativeInt _ -> Error IntFoldRejection.TargetWidth

[<RequireQualifiedAccess>]
type NumericLiteralValue =
    | Integral of IntValue
    | Float of double
    | Float32 of single
    | Decimal of decimal

/// Reasons why a numeric literal cannot be converted to a compile-time constant.
[<RequireQualifiedAccess>]
type NumericLiteralRejection =
    /// Custom numeric literals (e.g., 52I) or reserved suffixes that require runtime module calls.
    | CustomLiteral
    /// The literal's value exceeds the range of its type (e.g., 300uy or -1uy).
    | OutOfRange
    /// The token is not a numeric literal, or has an invalid NumericKind value.
    | NotNumeric

module NumericLiterals =

    /// Maps a NumericBase enum to a numeric radix (2, 8, 10, or 16).
    let private baseRadix (numBase: NumericBase) : int =
        match numBase with
        | NumericBase.Hex -> 16
        | NumericBase.Octal -> 8
        | NumericBase.Binary -> 2
        | _ -> 10

    /// Strips base prefixes (0x, 0o, 0b), type suffixes (u, l, y, s, n), and underscores from an integer literal.
    let private intDigits (numBase: NumericBase) (text: string) : string =
        let mutable hi = text.Length

        while hi > 0
              && (
                  match Char.ToLowerInvariant text.[hi - 1] with
                  | 'u'
                  | 'l'
                  | 'y'
                  | 's'
                  | 'n' -> true
                  | _ -> false
              ) do
            hi <- hi - 1

        let lo = if numBase = NumericBase.Decimal then 0 else 2
        text.Substring(lo, hi - lo).Replace("_", "")

    /// Returns the NumericKind for a token, or ValueNone if the token is not numeric.
    let numericKindOf (token: Token) : NumericKind voption =
        if TokenInfo.isNumeric token then
            ValueSome(TokenInfo.numericKind token)
        else
            ValueNone

    /// Strips a trailing suffix (such as 'f' or 'm') case-insensitively.
    let private stripSuffix (suffix: string) (text: string) : string =
        if text.EndsWith(suffix, StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    /// Parses a numeric literal token's raw source text into its evaluated constant value.
    let parseNumericLiteral (token: Token) (text: string) : Result<NumericLiteralValue, NumericLiteralRejection> =
        if not (TokenInfo.isNumeric token) then
            Error NumericLiteralRejection.NotNumeric
        else
            let numBase = TokenInfo.numericBase token
            let radix = baseRadix numBase
            let kind = TokenInfo.numericKind token

            let project () =
                match IntKind.ofNumericKind kind with
                | ValueSome k -> Ok(NumericLiteralValue.Integral(IntValue.parse k (intDigits numBase text) radix))
                | ValueNone ->

                    match kind with
                    // A hexadecimal, octal or binary float literal (`0x3f800000lf`, `0x3FF0000000000000LF`)
                    // spells the IEEE bit pattern of its value.
                    | NumericKind.IEEE64 when numBase <> NumericBase.Decimal ->
                        let bits = Convert.ToUInt64(intDigits numBase (stripSuffix "LF" text), radix)
                        Ok(NumericLiteralValue.Float(BitConverter.Int64BitsToDouble(int64 bits)))
                    | NumericKind.IEEE32 when numBase <> NumericBase.Decimal ->
                        let bits = Convert.ToUInt32(intDigits numBase (stripSuffix "lf" text), radix)
                        Ok(NumericLiteralValue.Float32(BitConverter.Int32BitsToSingle(int32 bits)))
                    | NumericKind.IEEE64 ->
                        Ok(NumericLiteralValue.Float(Double.Parse(text, CultureInfo.InvariantCulture)))
                    | NumericKind.IEEE32 ->
                        Ok(
                            NumericLiteralValue.Float32(
                                Single.Parse(stripSuffix "f" text, CultureInfo.InvariantCulture)
                            )
                        )
                    | NumericKind.Decimal ->
                        Ok(
                            NumericLiteralValue.Decimal(
                                Decimal.Parse(stripSuffix "m" text, NumberStyles.Float, CultureInfo.InvariantCulture)
                            )
                        )
                    | NumericKind.BigIntegerQ
                    | NumericKind.BigIntegerR
                    | NumericKind.BigIntegerZ
                    | NumericKind.BigIntegerI
                    | NumericKind.BigIntegerN
                    | NumericKind.BigIntegerG
                    | NumericKind.ReservedNumericLiteral -> Error NumericLiteralRejection.CustomLiteral
                    | _ -> Error NumericLiteralRejection.NotNumeric

            try
                project ()
            with
            | :? OverflowException
            | :? FormatException -> Error NumericLiteralRejection.OutOfRange
