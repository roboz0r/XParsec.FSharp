module AttributesAfterLet

let [<A>] f x = x + 1

let rec [<A>] g x = h x
and [<B>] h y = g y

let [<A>] internal fi x = x

let [<Literal>] Lit = 3

let [<A>] inline fin x = x

let [<A>] mutable mx = 1

let [<A>] private fp x = x

let [<A>] (|P|_|) x = Some x

let [<A>] [<B>] fm x = x

let [<A; B>] a, b = 1, 2

[<A>]
let [<B>] both x = x

[<A>]
let rec [<B>] first x = second x
and second x = first x

type C() =
    let [<A>] cf x = x
    static let [<A>] sf x = x
    member _.M x = cf x + sf x

type D() =
    let mutable v = 0
    member _.P with [<A>] get () = v and [<B>] set x = v <- x

let outer () =
    let [<A>] lf x = x
    let rec [<A>] lg x = lh x
    and [<B>] lh y = lg y
    lf 1
