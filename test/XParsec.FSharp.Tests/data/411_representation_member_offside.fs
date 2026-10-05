module MyModule

// A member left of the first token after `=` is offside of the type body and ends the
// declaration (FS0010 in each case).

type R =
        { X: int }
    member this.Y = this.X

type U =
        | A of int
        | B
    member this.Y = 1

type R2 =
        private
            { X2: int }
    member this.Y = this.X2

let good = 1
