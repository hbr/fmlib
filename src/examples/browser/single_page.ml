open Fmlib_browser


(* Pages
 * ======================================================================
 *)

module Home_page =
struct
    let view (): 'a Html.t =
        let open Html in
        let open Attribute in
        ul
            []
            [
                li [] [a [href "#counter"] [text "Counter"]];
                li [] [a [href "#clock"] [text "Digital clock"]];
                li [] [a [href "#die"] [text "Rolling die"]];
                li []
                    [a [href "#message"] [
                         text "Send message to Javascript"]
                    ];
                li [] [a [href "#events"] [text "Global events"]];
                li [] [a [href "#input"] [text "Input"]];
                li [] [a [href "#http"] [text "Http request"]];
            ]
end


module Not_found_page =
struct
    let view (url: Url.t): 'a Html.t =
        let open Html in
        let open Attribute in
        let fragment =
            match url.fragment with
            | Some f ->
                f
            | None ->
                assert false
                (* cannot happen, without fragment we redirect to Home page *)
        in
        let message = Printf.sprintf "Page #%s is not available." fragment in
        let home_url = {url with fragment = None} in
        div [] [
            p [] [ text message ];
            a [ href (Url.to_string home_url) ] [ text "Go to home page" ];
        ]
end






(* State and messages
 * ============================================================
 *)

type msg =
    | Clicked_link of Navigation.url_request
    | Changed_url of Url.t
    | Got_counter_msg of Single_page_counter.msg
    | Got_clock_msg of Single_page_clock.msg
    | Got_die_msg of Single_page_die.msg
    | Got_message_msg of Single_page_message.msg
    | Got_events_msg of Single_page_events.msg
    | Got_input_msg of Single_page_input.msg
    | Got_http_msg of Single_page_http.msg

let counter_msg m  = Got_counter_msg m
let clock_msg m    = Got_clock_msg m
let die_msg m      = Got_die_msg m
let message_msg m  = Got_message_msg m
let events_msg m   = Got_events_msg m
let input_msg m    = Got_input_msg m
let http_msg m     = Got_http_msg m


type page =
    | Not_found
    | Home
    | Counter
    | Clock
    | Die
    | Message
    | Events
    | Input
    | Http


type state =
    {
        key: msg Navigation.key;
        url: Url.t;
        page: page;
        counter: Single_page_counter.state;
        clock:   Single_page_clock.state;
        die:     Single_page_die.state;
        message: Single_page_message.state;
        events:  Single_page_events.state;
        input:   Single_page_input.state;
        http:    Single_page_http.state;
    }




(* Init function
 * ======================================================================
 *)


let change_page (url: Url.t) (state: state): state =
        let open Url.Parser in
        match
            parse
                ( s "fmlib" </>
                  s "webapp" </>
                  s "single_page.html" </>
                  fragment
                      ( function
                        | None -> Home
                        | Some "counter" -> Counter
                        | Some "clock" -> Clock
                        | Some "die" -> Die
                        | Some "message" -> Message
                        | Some "events" -> Events
                        | Some "input" -> Input
                        | Some "http" -> Http
                        | _ -> Not_found
                      )
                )
                url
        with
        | None ->
            Printf.sprintf "unknown internal url %s" (Url.to_string url)
            |> debug;
            assert false
            (* Cannot happen, the server ensures that the URL path is valid *)
        | Some page ->
            {state with url; page}


let init
        (url: Url.t)
        (key: msg Navigation.key)
    : (state * msg Command.t) Decoder.t
    =
    let counter, counter_cmd = Single_page_counter.init
    and clock, clock_cmd     = Single_page_clock.init
    and die, die_cmd         = Single_page_die.init
    and message, message_cmd = Single_page_message.init
    and events, events_cmd   = Single_page_events.init
    and input, input_cmd     = Single_page_input.init
    and http, http_cmd       = Single_page_http.init
    in
    let state =
        {
            key;
            url;
            page = Home;
            counter;
            clock;
            die;
            message;
            events;
            input;
            http
        }
    in
    let cmd =
        Command.batch
            [
                Command.map counter_msg counter_cmd;
                Command.map clock_msg clock_cmd;
                Command.map die_msg die_cmd;
                Command.map message_msg message_cmd;
                Command.map events_msg events_cmd;
                Command.map input_msg input_cmd;
                Command.map http_msg http_cmd;
            ]
    in
    (change_page url state, cmd) |> Decoder.return




(* Update function
 * ======================================================================
 *)


let update (state: state): msg -> state * msg Command.t =
    function
    | Clicked_link (External e) ->
        state, Command.load e

    | Clicked_link (Internal i) ->
        let url = Url.to_string i
        in
        state,
        if String.starts_with ~prefix:"/fmlib/webapp" i.path then
            Command.push_url state.key url
        else
            Command.load url

    | Changed_url url ->
        change_page url state, Command.none

    | Got_counter_msg m ->
        let counter, cmd = Single_page_counter.update state.counter m in
        {state with counter}, Command.map counter_msg cmd

    | Got_clock_msg m ->
        let clock, cmd = Single_page_clock.update state.clock m in
        {state with clock}, Command.map clock_msg cmd

    | Got_die_msg m ->
        let die, cmd = Single_page_die.update state.die m in
        {state with die}, Command.map die_msg cmd

    | Got_message_msg m ->
        let message, cmd = Single_page_message.update state.message m in
        {state with message}, Command.map message_msg cmd

    | Got_events_msg m ->
        let events, cmd = Single_page_events.update state.events m in
        {state with events}, Command.map events_msg cmd

    | Got_input_msg m ->
        let input, cmd = Single_page_input.update state.input m in
        {state with input}, Command.map input_msg cmd

    | Got_http_msg m ->
        let http, cmd = Single_page_http.update state.http m in
        {state with http}, Command.map http_msg cmd





(* View functions
 * ======================================================================
 *)

let view_page (page: msg Html.t) (headline: string): msg Html.t =
    (* Display the virtual page in the following common format:

       -----------------------------------------------------------------
       | Home button                             link to documentation |
       |                                                               |
       | Headline for the virtual page                                 |
       |                                                               |
       | Virtual page                                                  |
       |                                                               |
       -----------------------------------------------------------------
    *)
    let open Html in
    let open Attribute in
    let doc_ref =
        "https://hbr.github.io/fmlib/odoc/fmlib_browser/index.html"
    in
    div []
        [
            nav [margin "20px"]
                [
                    a [href "single_page.html"] [text "Home"];
                    a
                        [ href doc_ref;
                          style "float" "right" ]
                        [ text "documentation" ];
                ];
            div
                [ margin "20px";
                  border_style "solid";
                  padding "0px 20px 20px 20px" ]
                [ h2 [] [text headline];
                  page ];
        ]


let page_html (state: state): msg Html.t * string =
    (* The html and the title of the current page. *)
    let map f html = Html.map f html
    in
    match state.page with
    | Not_found ->
        Not_found_page.view state.url,
        "Not found"

    | Home ->
        Home_page.view (),
        "Home"

    | Counter ->
        Single_page_counter.view state.counter
        |> map counter_msg,
        "Counter"

    | Clock ->
        Single_page_clock.view state.clock
        |> map clock_msg,
        "Clock"

    | Die ->
        Single_page_die.view state.die
        |> map die_msg,
        "Rolling die"

    | Message ->
        Single_page_message.view state.message
        |> map message_msg,
        "Send message"

    | Events ->
        Single_page_events.view state.events
        |> map events_msg,
        "Global events"

    | Input ->
        Single_page_input.view state.input
        |> map input_msg,
        "Input"

    | Http ->
        Single_page_http.view state.http
        |> map http_msg,
        "Http request"


let view (state: state): msg Html.t * string =
    let page, title = page_html state in
    view_page page title, title




(* Subscriptions function
 * ======================================================================
 *)

let subscriptions (state: state): msg Subscription.t =
    Subscription.(batch [
        Single_page_clock.subscriptions state.clock
        |> map clock_msg;

        Single_page_message.subscriptions state.message
        |> map message_msg;

        if state.page = Events then
            Single_page_events.subscriptions state.events |> map events_msg
        else
            none
    ])


(* Application
 * ======================================================================
 *)

let _ =
    application
        "single_page_app"
        init
        view
        subscriptions
        update
        (fun req -> Clicked_link req)
        (fun url -> Changed_url url)
