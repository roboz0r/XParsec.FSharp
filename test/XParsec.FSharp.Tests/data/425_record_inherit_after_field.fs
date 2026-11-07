module MyModule

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    // Rejected: the `inherit` clause must open the record expression.
    new(n: int) = { Y = n; inherit B(1) }

    member _.Z = 1
