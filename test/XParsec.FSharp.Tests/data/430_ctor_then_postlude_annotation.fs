module MyModule

// The type annotation after a `then` postlude belongs to the postlude.

type D(v: int) =
    member _.V = v
    new() = D(1) then () : unit
