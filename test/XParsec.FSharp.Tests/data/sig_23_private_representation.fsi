module PrivateRepresentation

type R = private { x: int }

type D = internal | A | B of int

type T =
    new: unit -> T
    member P: int with get, private set
