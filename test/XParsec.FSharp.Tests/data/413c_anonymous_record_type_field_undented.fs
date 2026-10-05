module MyModule

// An anonymous record type field undented before the first field is offside (FS0010).

let y: {|
            A: int
        B: int
    |} = {| A = 1; B = 2 |}
