module MyModule

// F# parses a record `inherit` clause and `e1 then e2` in any expression position; its type
// checker accepts them only as the initialisation of a constructor body (FS0788, FS0743).

type B(x: int) =
    member _.X = x

let x = { inherit B(1); Y = 2 }

let y = { inherit B(1) }

let f () = printfn "a" then printfn "b"

type D =
    inherit B
    val Y: int
    val Z: int

    new(n: int) =
        if n = 0 then { inherit B(0); Y = 0; Z = 1 }
        { inherit B(n); Y = n; Z = n }
