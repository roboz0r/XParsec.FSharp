module MyModule

// An array literal after the type of `new` or `inherit` is the constructor argument.

type C(x: int[]) =
    member _.X = x

let c = new C [|1|]
let d = new C[|2|]

type D =
    inherit C
    new() = { inherit C [|1|] }
