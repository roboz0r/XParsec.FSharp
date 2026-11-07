module MyModule

type B(x: obj) =
    member _.X = x

type D =
    inherit B

    // Rejected: the argument after the type of `inherit` is a constant or a delimited expression.
    new() = { inherit B new obj() }

    member _.Z = 1
