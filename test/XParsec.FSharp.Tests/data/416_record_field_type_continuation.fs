module MyModule

// A record field type continues on later lines right of the field's start column.

let f (x: {| A: int
               list; B: string |}) = x

let g (x: {| A: Map<
               int,
               string>; B: string |}) = x

type R = { A: int
             list; B: string }

type R2 = { A2: Map<
              int,
              string>; B2: string }

type R3 =
    {
        A3: int
          list
        B3: string
    }
