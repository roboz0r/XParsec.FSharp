module MyModule

// Two anonymous record type fields on one line need a `;` between them (FS0010).

let f (x: {| A: int B: int |}) = x.A
