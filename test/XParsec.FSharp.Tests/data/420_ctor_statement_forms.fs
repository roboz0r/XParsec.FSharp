module MyModule

// Ordinary expressions in statement position of constructor branches.

type Data = { V: int }

type R =
    val X: int

    new(n: int) =
        if n = 0 then
            new System.Object() |> ignore
        if n = 1 then
            new System.Object() |> ignore; ()
        { X = n }

    new(n: int, m: int) =
        if n = 0 then
            { V = 0 } |> ignore
            ignore { V = 1 }; ()
        if n = 1 then
            () : unit
        { X = n + m }

    new(n: string) =
        if n = "" then
            let rec f x = if x = 0 then () else g (x - 1)
            and g x = f x
            f 3
        { X = 0 }
