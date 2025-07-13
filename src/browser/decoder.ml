include Fmlib_js.Base.Decode


let run (decode: 'a t) (v: Value.t): 'a option =
    decode v
