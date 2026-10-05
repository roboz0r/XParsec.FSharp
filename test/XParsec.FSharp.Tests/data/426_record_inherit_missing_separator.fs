module MyModule

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    // Rejected: a field initialiser on the line of the `inherit` clause needs a `;`.
    new(n: int) = { inherit B(1) Y = n }

    member _.Z = 1
