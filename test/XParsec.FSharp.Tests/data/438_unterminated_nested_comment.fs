// End of input inside a nested block comment reports at the outermost opener (FS0516).
let a = 1
(* outer (* inner *)
let b = 2
