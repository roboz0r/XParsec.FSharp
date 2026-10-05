module MyModule

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    // Rejected: a `then` undented left of the body it continues.
    new(n: int) = { inherit B(1); Y = n }
        then printfn "a"

    member _.Z = 1
