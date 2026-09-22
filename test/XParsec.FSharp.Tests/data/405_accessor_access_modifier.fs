module MyModule

type T() =
    let mutable v = 0

    member this.P
        with get () = v
        and private set (x: int) = v <- x

    member this.Q
        with private get () = v
        and internal set (x: int) = v <- x

    member val R = 1 with get, set
