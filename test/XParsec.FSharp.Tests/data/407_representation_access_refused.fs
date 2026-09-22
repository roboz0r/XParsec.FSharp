module MyModule

// An object-model type writes its accessibility on the type name, so a modifier after `=` is
// refused. The modifier token is retained either way — on the recovered abbreviation for `A`,
// in a skipped module element for `B` — and the declaration after each one still parses.

type A = private member _.M = 1

type B(x: int) = private
    do printfn "%d" x

type Good = { y: int }
