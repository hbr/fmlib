open Js_of_ocaml
open Fmlib_std


module Value =
struct
    type t = Js.Unsafe.any

    let null: t =
        Js.(Unsafe.inject null)

    let undefined: t =
        Js.(Unsafe.inject undefined)

    let int (i: int): t =
        Js.Unsafe.inject i

    let float (v: float): t =
        Js.Unsafe.inject v

    let bool (b: bool): t =
        Js.(Unsafe.coerce (bool b))

    let string (s: string): t =
        Js.(Unsafe.coerce (string s))

    let _object (arr: (string * t) array): t =
        Js.Unsafe.obj arr

    let array (arr: t array): t =
        Js.(Unsafe.coerce (array arr))

    let function1 (f: t -> t): t =
        Js.Unsafe.inject f

    let function2 (f: t -> t -> t): t =
        Js.Unsafe.inject f
end




module Decode =
struct
    let str_boolean: Js.js_string Js.t =
        Js.string "boolean"

    let str_function: Js.js_string Js.t =
        Js.string "function"

    let str_string: Js.js_string Js.t =
        Js.string "string"

    let str_number: Js.js_string Js.t =
        Js.string "number"

    let is_function (v: Value.t): bool =
        Js.(typeof v == str_function)

    let is_boolean (v: Value.t): bool =
        Js.(typeof v == str_boolean)

    let is_string (v: Value.t): bool =
        Js.(typeof v == str_string)

    let is_number (v: Value.t): bool =
        Js.(typeof v == str_number)


    type 'a t = Value.t -> 'a option


    let return (a: 'a): 'a t =
        fun _ -> Some a


    let fail: 'a t =
        fun _ -> None


    let (let* ) (m: 'a t) (f: 'a -> 'b t): 'b t =
        fun v ->
        Option.(
            let* a = m v in
            f a v
        )


    let (>>=) = (let* )


    let (</>) (p: 'a t) (q: 'a t): 'a t =
        fun v ->
        match p v with
        | None ->
            q v
        | Some _ as r ->
            r


    let map (f: 'a -> 'b) (m: 'a t): 'b t =
        let* a = m in
        return (f a)


    let option (decode: 'a t): 'a option t =
        fun obj ->
        let open Js in
        match
            Opt.to_option (some (Unsafe.coerce obj))
        with
        | None ->
            Some None
        | Some v ->
            match decode v with
            | None ->
                None
            | Some a ->
                Some (Some a)


    let float: float t =
        fun v ->
        if is_number v then
            Some (Js.(Unsafe.coerce v)##valueOf ())
            (* [valueOf] returns a primitive value. *)
        else
            None


    let int: int t =
        let* v = float in
        let open Float in
        let i = to_int v in
        if equal v (of_int i) then
            return i
        else
            fail


    let bool: bool t =
        fun v ->
        if is_boolean v then
            Some Js.(Unsafe.coerce v |> to_bool)
        else
            None


    let string: string t =
        fun v ->
        if is_string v then
            Some Js.(Unsafe.coerce v |> to_string)
        else
            None


    let _function: (Value.t array -> Value.t) t =
        fun v ->
        if is_function v then
            Some (fun args -> Js.Unsafe.fun_call v args)
        else
            None


    let field (name: string) (decode: 'a t): 'a t =
        fun obj ->
        let open Js in
        Option.(
            let* v = Optdef.to_option (Unsafe.get obj (string name)) in
            decode v
        )


    let array (decode: 'a t): 'a array t =
        fun obj ->
        let open Js in
        if Unsafe.global##._Array##isArray obj then
            let js_arr = (Unsafe.coerce obj) in
            let len = js_arr##.length in
            let rec extract i lst =
                if i = len then
                    Some (Array.of_list (List.rev lst))
                else
                    let open Option in
                    let* e = array_get js_arr i |> Optdef.to_option in
                    let* a = decode e in
                    extract (i + 1) (a :: lst)
            in
            extract 0 []
        else
            None
end



module Main =
struct
    let raise_js (message: string): 'a =
        Js.(
            raise_js_error
                (new%js
                    error_constr
                    (Js.string message)
                )
        )



    let log_string (str: string): unit =
        Js.(Unsafe.global##log (string str))


    let log_value (value: Value.t): unit =
        Js.(Unsafe.global##log value)


    let export (obj: (string * Value.t) array): unit =
        Js.export_all (Value._object obj)

    let named_export (name: string) (obj: (string * Value.t) array): unit =
        Js.export name (Value._object obj)


    let test (f: Value.t -> Value.t): unit =
        Js.export_all
            Value.(_object [|"make", function1 f |])


    let node_module
            (decode: 'state Decode.t)
            (decode_message: 'msg Decode.t)
            (f: 'state -> (Value.t -> unit) -> 'msg -> unit)
        : unit
        =
        Js.export_all
            (object%js
                method make (data: Value.t) (callback: Value.t): Value.t =
                    let callback: Value.t array -> Value.t =
                        match Decode._function callback with
                        | Some callback ->
                            callback
                        | None ->
                            raise_js "provided callback is not a function"
                    and state =
                        match decode data with
                        | Some state ->
                            state
                        | None ->
                            raise_js "cannot Decode.t input data"
                    in
                    let f = f state (fun v -> callback [|v|] |> ignore)
                    in
                    Js.Unsafe.coerce
                        (object%js
                            method postMessage (msg: Value.t): Value.t =
                                match decode_message msg with
                                | None ->
                                    log_string "cannot Decode.t message";
                                    log_value msg;
                                    raise_js "cannot Decode.t message"
                                | Some msg ->
                                    f msg;
                                    Value.undefined
                        end)
            end)
end
