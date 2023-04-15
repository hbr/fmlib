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

    let function3 (f: t -> t -> t -> t): t =
        Js.Unsafe.inject f


    let stringify (v: t): t option =
        try
            Some Js.(Unsafe.coerce (_JSON##stringify v))
        with _ ->
            None

    let parse (v: t): t option =
        try
            Some Js.(_JSON##parse (Unsafe.coerce v))
        with _ ->
            None
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


    let value: Value.t t =
        fun v -> Some v


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

    let null (a: 'a): 'a t =
        fun obj ->
        if obj == Value.null then
            Some a
        else
            None


    let undefined (a: 'a): 'a t =
        fun obj ->
        if obj == Value.undefined then
            Some a
        else
            None


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


    let _method: (Value.t -> Value.t array -> Value.t) t =
        fun v ->
        if is_function v then
            Some (fun obj args -> Js.Unsafe.call v obj args)
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


    let option (decode: 'a t): 'a option t =
        map Option.return decode
        </>
        null None

end



module Main =
struct

    (* General functions
     * =================
     *)

    type js_error = Js.Js_error.t


    let of_exception (exn: exn): js_error option =
        Js.Js_error.of_exn exn


    let raise_js_error (js_error: js_error): 'a =
        Js.Js_error.raise_ js_error


    let raise_js (message: string): 'a =
        let js_msg = Js.string message in
        Js.(
            new%js
                error_constr
                js_msg
            |> Js_error.of_error
            |> Js_error.raise_
        )



    let log_string (str: string): unit =
        Js.(Unsafe.global##.console##log (string str))


    let log_value (value: Value.t): unit =
        Js.(Unsafe.global##.console##log value)


    let export (obj: (string * Value.t) array): unit =
        Js.export_all (Value._object obj)


    let make_global (name: string) (v: Value.t): unit =
        Js.(Unsafe.(set global (Js.string name) v))

    let get_global (name: string): Value.t option =
        let open Js in
        Unsafe.(get global (Js.string name)) |> Opt.to_option


    let new_global (cname: string) (args: Value.t array): Value.t =
        match get_global cname with
        | None ->
            assert false
        | Some constr ->
            Js.Unsafe.new_obj constr args



    (* Helper functions for Node module and browser application
     * ========================================================
     *)


    let decode_callback (cb: Value.t) (err: string): Value.t -> unit =
        match Decode._function cb with
        | None ->
            raise_js err
        | Some cb ->
            fun v -> cb [|v|] |> ignore


    let decode_data
            (dec: 'a Decode.t) (data: Value.t) (err: string)
        : 'a
        =
        match dec data with
        | None ->
            raise_js err
        | Some state ->
            state




    (* Node module
     * ===========
     *)


    type ('state,'msg) node_function =
        'state -> (Value.t -> unit) -> 'msg -> unit


    let node_module
            (decode: 'state Decode.t)
            (msg_decode: 'msg Decode.t)
            (node_function: ('state, 'msg) node_function)
        : unit
        =
        let js_function data callback =
            let callback =
                decode_callback callback
                    "provided callback is not a function"
            and data =
                decode_data decode data
                    "cannot decode input data"
            in
            let f =
                node_function data callback
            in
            Value.function1
                (fun msg ->
                     match msg_decode msg with
                     | None ->
                         log_string "cannot decode message";
                         log_value msg;
                         Value.undefined
                     | Some msg ->
                         f msg;
                         Value.undefined)
        in
        export [| "init", Value.function2 js_function |]







    (* Browser Application
     * ===================
     *)


    type ('state, 'msg) browser_function =
        'state -> string option -> (Value.t -> unit) -> 'msg -> unit


    let browser_application
            (app_name: string)
            (state_decode: 'state Decode.t)
            (msg_decode: 'msg Decode.t)
            (browser_function: ('state, 'msg) browser_function)
        : unit
        =
        let js_function state element callback =
            let callback =
                decode_callback callback
                    "provided callback is not a function"
            and state =
                decode_data state_decode state
                    "cannot decode state"
            and element =
                decode_data Decode.(option string) element
                    "cannot decode a nullable element id"
            in
            let f =
                browser_function state element callback
            in
            Value.function1
                (fun msg ->
                     match msg_decode msg with
                     | None ->
                         log_string "cannot decode message";
                         log_value msg;
                         Value.undefined
                     | Some msg ->
                         f msg;
                         Value.undefined)
        in
        make_global
            app_name
            (Value.function3 js_function)
end
