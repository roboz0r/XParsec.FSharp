module MyModule

// An `if ... then statement` without `else` runs before the initialisation that ends the body.

[<Struct>]
type Rational =
    val Numerator: bigint
    val Denominator: bigint

    private new(n: bigint, d: bigint) =
        if d.IsZero then
            invalidArg "d" "Rational denominator must be nonzero"

        let sign = if d.Sign < 0 then -1I else 1I
        let n' = n * sign
        let d' = d * sign

        {
            Numerator = n'
            Denominator = d'
        }

type R =
    val X: int

    new(n: int) =
        if n = 0 then printfn "zero"
        printfn "more"
        { X = n }

    new(n: int, m: int) =
        if n = 0 then
            if m = 0 then
                if n = m then
                    ()
        { X = n }
