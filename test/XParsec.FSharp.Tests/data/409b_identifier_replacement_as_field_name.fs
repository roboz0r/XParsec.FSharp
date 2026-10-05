module MyModule

// An identifier replacement is a constant, so it is refused as a record field name (FS0609).

let r = {| __LINE__ = 1 |}
