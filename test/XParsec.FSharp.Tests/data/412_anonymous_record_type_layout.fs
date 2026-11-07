module MyModule

// Anonymous record type fields are separated by `;` or by a newline at the first field's
// column, with an optional trailing `;`.

let halvesOf
    (x: int)
    : {|
          A: int
          B: int
      |} =
    {| A = x; B = x |}

type U =
    | New of
        {|
            A: int
            B: int
        |}
    | Other

let x: {|
        A: int
        B: int
    |} = {| A = 1; B = 2 |}

let f (x: {| A: int; |}) = x.A

let g (x: {| A: int; B: string; |}) = x.A

let h (x: {| A: int list
             B: {| C: int
                   D: string option |} |}) = x.A

let i (x: {| A: int; B: int
             C: int |}) = x.A

let j (x: {| A: int -> int
             B: int * int |}) = x.A

let k: {|
    A: int
    B: int
|} = {| A = 1; B = 2 |}
