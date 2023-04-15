open Fmlib_js.Base.Main

let attempt (str: string) (f: unit -> 'a) (cleanup: unit -> unit): 'a =
    try
        f ()
    with
    | Assert_failure (file, line, col) ->
        let msg =
            String.concat
                ""
                ["assertion failed file: "
                ; file
                ; " line: "; string_of_int line
                ; " col: ";   string_of_int col
                ]
        in
        cleanup ();
        log_string str;
        log_string msg;
        raise_js str

    | exn ->
        cleanup ();
        log_string str;
        match of_exception exn with
        | None ->
            log_string "Uncaught ocaml exception";
            raise exn
        | Some js_error ->
            log_string "Uncaught javascript exception";
            raise_js_error js_error
