module MyModule

// Members after a record or union representation align with the first token after `=`,
// which is the representation access modifier when one is present.

type R =
    private
        {
            X: int
        }

    member this.Y = this.X

type R2 =
    private { X2: int }
    member this.Y = this.X2

type U =
    private
        | A of int
        | B

    member this.Y = 1

type U2 =
    internal
        | C
        | D
    static member Z = 1
    member this.W = 2

type U3 =
    private | E | F
    member this.Y = 1

type R3 =
    internal
        { X3: int }
    interface System.IDisposable with
        member this.Dispose() = ()

type R4 =
    private
        { X4: int }
    with
        member this.Y = this.X4

type R5 =
    { X5: int }
    member this.Y = this.X5
