type prevent = Prevent of bool

type stop    = Stop    of bool


let prevent:    prevent = Prevent true
let no_prevent: prevent = Prevent false

let stop:    stop   = Stop true
let no_stop: stop   = Stop false
