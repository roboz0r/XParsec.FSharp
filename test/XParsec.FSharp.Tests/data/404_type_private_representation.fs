module MyModule

type R = private { x: int }

type S = internal { mutable y: string }

type D = private | A | B of int

type E = internal Single of int | Pair of int * int
