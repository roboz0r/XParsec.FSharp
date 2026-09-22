module MyModule

type B(n: int) =
    member this.N = n

type D =
    inherit B
    val x: int
    new() = { inherit B(9); x = 1 }
    new(k: int) = { inherit B; x = k }
    new(k: int, m: int) =
        { inherit B
          x = k + m }
