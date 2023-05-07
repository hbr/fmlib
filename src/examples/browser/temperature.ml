open Fmlib_browser

type state = {
    ok: bool;
    celcius: string;
    fahrenheit: string;
}

type msg =
    | Celcius of string
    | Fahrenheit of string



let print_float (v: float): string =
    Printf.sprintf "%.1f" v


let fahrenheit_to_celcius (fahrenheit: float): string =
    (fahrenheit -. 32.0) *. (5.0 /. 9.0) |> print_float


let celcius_to_fahrenheit (celcius: float): string =
    32.0 +. celcius *. (9.0 /. 5.0) |> print_float


let set_temperature (state: state) (is_celcius: bool) (value: string): state =
    try
        let v = float_of_string value in
        if is_celcius then
            {
                ok = true;
                celcius = value;
                fahrenheit = celcius_to_fahrenheit v;
            }
        else
            {
                ok = true;
                celcius = fahrenheit_to_celcius v;
                fahrenheit = value;
            }
    with _ ->
        if is_celcius then
            {
                state with
                ok = false;
                celcius = value;
            }
        else
            {
                state with
                ok = false;
                fahrenheit = value;
            }


let init: state =
    {
        ok = true;
        celcius = "20";
        fahrenheit = celcius_to_fahrenheit 20.0;
    }



let update (state: state): msg -> state =
    function
    | Celcius celcius ->
        set_temperature state true celcius
    | Fahrenheit fahrenheit ->
        set_temperature state false fahrenheit



let view (state: state): 'msg Html.t =
    let open Html in
    let open Attribute in
    let message is_celcius v =
        if is_celcius then
            Celcius v
        else
            Fahrenheit v
    and temperature_field txt v f =
        div [] [
            label [] [
                input [ attribute "type" "text"
                      ; value v
                      ; on_input f]
                    []
              ; text " "
              ; text txt
            ]
        ]
    and error =
        if state.ok then
            []
        else
            [div [color "red"] [text "value must be a number"]]
    in
    div [] (
        h1 [] [text "Temperature Converter"]
        ::
        temperature_field "Celcius" state.celcius (message true)
        ::
        temperature_field "Fahrenheit" state.fahrenheit (message false)
        ::
        error
      )


let _ = sandbox init view update
