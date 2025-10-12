open Fmlib_browser

type state =
    {
        password1: string;
        password2: string;
        text_input: string;
        username: string;
        slider_value: string;
    }


type msg =
    | Got_password1 of string
    | Got_password2 of string
    | Got_text_input of string
    | Got_username of string
    | Got_slider_value of string


let init: state * msg Command.t =
    let state =
        {
            password1 = "";
            password2 = "";
            text_input = "";
            username = "";
            slider_value = "";
        }
    in
    (state, Command.none)


let update (state: state) (msg: msg): state * msg Command.t =
    let state =
        match msg with
        | Got_password1 password1 ->
            {state with password1}
        | Got_password2 password2 ->
            {state with password2}
        | Got_text_input text_input ->
            {state with text_input}
        | Got_username username ->
            {state with username}
        | Got_slider_value slider_value ->
            {state with slider_value}
    in
    (state, Command.none)


let view (state: state): msg Html.t =
    let open Fmlib_std in
    let open Html in
    let open Attribute in
    let password_ok =
        state.password1 = state.password2
    in
    div []
        [
            h3 [] [text "Text input"];
            input
                [
                    type_ "text";
                    placeholder "Text to reverse";
                    value state.text_input;
                    on_input (fun i -> Got_text_input i);
                ] [];
            p []
                [
                    text "reversed text: ";
                    text (String.reverse state.text_input);
                ];
            h3 [] [text "Slider"];
            input
                [
                    type_ "range";
                    min "0";
                    max "10";
                    step 0.5;
                    value state.slider_value;
                    on_input (fun i -> Got_slider_value i);
                ] [];
            p [] [text "slider value: "; text state.slider_value];
            h3 [] [text "Password check"];
            input
                [
                    type_ "text";
                    placeholder "user name";
                    value state.username;
                    on_input (fun i -> Got_username i);
                ] [];
            input
                [
                    type_ "password";
                    placeholder "password";
                    value state.password1;
                    on_input (fun i -> Got_password1 i);
                ] [];
            input
                [
                    type_ "password";
                    placeholder "re enter password";
                    value state.password2;
                    on_input (fun i -> Got_password2 i);
                ] [];
            p
                (if password_ok then [color "green"] else [color "red"])
                (
                    if password_ok then
                        [text "Ok"]
                    else
                        [text "passwords don't match"]
                );
        ]
