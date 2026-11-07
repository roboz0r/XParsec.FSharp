module MyModule

// Match arms ending in initialisations, delegation written as an application or as `new T arg`,
// and the forms of a record's `inherit` clause.

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    new(n: int) =
        match n with
        | 0 -> { inherit B(1); Y = n }
        | m when m > 10 -> new D(0)
        | _ -> D(1)

    new(n: string) = { inherit B 1; Y = 2 }
    new(n: float) = { inherit B (1); Y = 2 }
    new(n: char) = { inherit B(1); Y = 2; }
    new(n: int64) = new D 1

    new(n: bool) = { inherit B
                     Y = 1 }

type E =
    inherit B
    new() = { inherit B(1) }
    new(n: int) = { inherit B(n); }

type PairOfIntegers(x: int, y: int) =
    new(x) = PairOfIntegers(x, x)
