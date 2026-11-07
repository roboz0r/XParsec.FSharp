module RepresentationAccessMembers

// Members after a record or union representation align with the representation access
// modifier when one is present.

type R =
    private
        {
            X: int
        }

    member Y: int

type R2 =
    private { X2: int }
    member Y: int

type U =
    private
        | A of int
        | B

    member Y: int

val make: int -> R
