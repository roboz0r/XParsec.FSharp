module MyModule

// `then` after the operand of `yield` or `return` continues the enclosing expression; after the
// operand of `do` or `do!` it continues the operand.

let f = seq { yield 1 then yield 2 }

let g = async { return 1 then printfn "a" }

let h = async { do printfn "a" then printfn "b" }

let k = async { do! async.Return() then () }

let m () = do () then ()
