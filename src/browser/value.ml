include Fmlib_js.Base.Value

let record = _object

let stringify (v: t): t =
    stringify v
    |> Option.get (* Fmlib_browser values are guaranteed to be serializable
                     by construction. *)
