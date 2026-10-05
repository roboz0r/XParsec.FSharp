module MyModule

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    // Rejected: `then` must follow an expression.
    new(n: int) = then printfn "a"

    member _.Z = 1
