// Block comments nest, and comment text is lexed as fsc lexes it.
(* outer (* inner *) still comment *)
let a = 1
(* d1 (* d2 (* d3 *) d2 *) d1 *)
let b = 2
(* line1
   (* line2
      line3 *)
   line4 *)
let c = 3
(* "*)" @"\*)" """ *) """ '"' *)
let d = 4
let e = (*) 2 3
let f = ( *) 2 3
(* (*) *)
(* // *)
// (*
(* ``a *)
(* $"{" *)
(* $@"\" *)
(* @$"{" *)
(* $"""{""" *)
(* +@"\" *)
(*
#if FOO *)
(*F# let g = 0 F#*)
(*IF-FSHARP let g = 0 ENDIF-FSHARP*)
(*IF-OCAML*)
(* outer F#*)
let g = $"a{ a (* } : ( *) }b"
(***)
(** doc (* nested *) *)
let h = 5
