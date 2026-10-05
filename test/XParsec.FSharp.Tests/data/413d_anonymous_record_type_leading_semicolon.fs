module MyModule

// An anonymous record type starts with a field, so a leading `;` is refused (FS0010).

let g (x: {| ; A: int |}) = x.A
