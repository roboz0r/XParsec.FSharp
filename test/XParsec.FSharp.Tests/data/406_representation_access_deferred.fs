module MyModule

// fsc admits an access modifier on any simple representation and then refuses two of them
// semantically: FS0566 on an abbreviation, FS0567 on an enum. The parse keeps the modifier so
// the front end can report it.

type A = private int

type E = private | A = 1 | B = 2
