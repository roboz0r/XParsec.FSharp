module MyModule

// The argument after the type of an object expression's `new` is an atomic expression, as after the
// type of `new` and `inherit`. Without `with` the braces hold a `new` expression.

type B(xs: int[]) =
    abstract M: unit -> int
    default _.M() = 1
    new(n: int) = B([||])

let a = { new B [|1|] with member _.M() = 2 }
let c = { new B [|1|] }
let d = { new B(1) }
let e = { new obj() }

type C(xs: int[]) =
    member _.Xs = xs

type G<'T>(xs: 'T[]) =
    member _.Xs = xs

let f = new C [||]
let g = new G<int> [||]

type D =
    inherit C
    new() = { inherit C [||] }

type E() =
    inherit C [||]
