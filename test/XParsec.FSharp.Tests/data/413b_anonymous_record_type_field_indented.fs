module MyModule

// An anonymous record type field indented past the first field continues the previous field
// type rather than starting a field (FS0010).

let x: {|
        A: int
          B: int
    |} = {| A = 1; B = 2 |}
