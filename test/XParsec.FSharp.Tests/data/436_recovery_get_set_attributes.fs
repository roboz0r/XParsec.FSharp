module RecoveryGetSetAttributes

type C() =
    member _.P with [<A>] oops () = 1
