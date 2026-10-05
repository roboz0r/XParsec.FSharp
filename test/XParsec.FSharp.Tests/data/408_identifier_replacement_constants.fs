module MyModule

// `__SOURCE_DIRECTORY__`, `__SOURCE_FILE__` and `__LINE__` are string constants in expression,
// attribute-argument and pattern position.

let a = __SOURCE_DIRECTORY__
let b = __SOURCE_FILE__
let c = __LINE__
let d = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "x")
let e = id __SOURCE_FILE__
let f = __LINE__ + "1"

[<Literal>]
let g = __SOURCE_DIRECTORY__

let h () =
    printfn "x"
    __LINE__

let i =
    match "x" with
    | __SOURCE_FILE__ -> 1
    | __LINE__ -> 2
    | _ -> 3

[<System.Obsolete(__SOURCE_FILE__)>]
let j = 1

let k = [ __LINE__; __SOURCE_FILE__ ]
let l = __SOURCE_DIRECTORY__.Length
let m = (__LINE__)

let n =
    [
        __SOURCE_FILE__
        __LINE__
    ]
