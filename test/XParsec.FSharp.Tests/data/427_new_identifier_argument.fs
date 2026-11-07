module MyModule

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    new(n: int) = { inherit B(1); Y = n }

    // Rejected: the argument of `new T arg` is a constant or a delimited expression.
    new(n: string) = new D n

    member _.Z = 1
