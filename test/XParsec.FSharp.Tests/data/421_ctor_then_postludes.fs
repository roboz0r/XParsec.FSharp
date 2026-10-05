module MyModule

// `then` postludes after a constructor initialisation, in branches and at the end of the body.

type B(x: int) =
    member _.X = x
    new() = B(0)

type D =
    inherit B
    val Y: int

    new(n: int) =
        if n = 0 then
            { inherit B(1); Y = 1 } then printfn "inner"
        else
            { inherit B(2); Y = 2 }

    new(n: string) =
        { inherit B(1); Y = 0 }
        then
            printfn "a"

    new(n: float) =
        { inherit B(1); Y = 0 }
        then printfn "a"
             printfn "b"

    new(n: bool) =
        { inherit B(1); Y = 0 } then
        printfn "a"

    new(n: char) =
        printfn "x"; { inherit B(1); Y = 1 } then printfn "inner"; printfn "z"

    new(n: int, m: int) =
        if n = m then { inherit B(1); Y = n } else { inherit B(2); Y = m }
        then printfn "a"

    new(n: int, m: string) =
        let x = n
        { inherit B(x); Y = x }
        then printfn "a"
