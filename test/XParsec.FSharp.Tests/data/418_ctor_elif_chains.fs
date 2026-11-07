module MyModule

// Conditional constructor initialisations chained with `elif`, `else if` and a multi-line
// collapsed `else if`.

type R =
    val X: int

    new(n: int) =
        if n = 0 then { X = 0 }
        elif n = 1 then { X = 1 }
        else { X = 2 }

    new(n: int, m: int) =
        if n = 0 then { X = 0 }
        else if n = 1 then { X = 1 }
        elif n = 2 then { X = 2 }
        else { X = m }

    new(n: string) =
        if n = "" then { X = 0 }
        else if n =
            "a" then { X = 1 }
        else { X = 2 }
