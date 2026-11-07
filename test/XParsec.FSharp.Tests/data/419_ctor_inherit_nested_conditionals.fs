module MyModule

// `{ inherit B(n); Y = n }` and `new D(...)` delegation in nested conditional branches.

type B(x: int) =
    member _.X = x

type D =
    inherit B
    val Y: int

    new(n: int) =
        if n = 0 then new D(1)
        else
            if n = 1 then new D(2)
            else
                if n = 2 then { inherit B(n); Y = n }
                else { inherit B(n + 1); Y = n }

    new(n: int, m: int) =
        if n = m then
            if n = 0 then
                { inherit B(0); Y = 0 }
            else
                new D(n)
        else
            { inherit B(m); Y = m }
