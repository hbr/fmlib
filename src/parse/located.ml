type 'a t = Position.range * 'a


let make (r: Position.range) (a: 'a): 'a t =
    r, a


let range (loc: 'a t): Position.range =
    fst loc


let value (loc: 'a t): 'a =
    snd loc


let start (loc: 'a t): Position.t =
    fst (fst loc)


let _end (loc: 'a t): Position.t =
    snd (fst loc)
