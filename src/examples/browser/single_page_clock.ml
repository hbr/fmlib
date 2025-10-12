open Fmlib_browser

type state =
    {
        time: Time.t;
        zone: Time.Zone.t;
    }


type msg = Got_time of Time.t | Got_time_zone of Time.Zone.t


let init: state * msg Command.t =
    let command =
        let open Command in
        batch
            [
                now (fun t -> Got_time t);
                time_zone (fun z -> Got_time_zone z)
            ]
    in
    ({time = Time.zero; zone = Time.Zone.utc}, command)


let update (state: state) (msg: msg): state * msg Command.t =
    match msg with
    | Got_time time ->
        ({state with time}, Command.none)
    | Got_time_zone zone ->
        ({state with zone}, Command.none)


let subscriptions (_state: state): msg Subscription.t =
    Subscription.every 1000 (fun t -> Got_time t)


let view (state: state): msg Html.t =
    let open Html in
    let open Attribute in
    let time tag time zone =
        div [margin "10px"; border_style "solid"; padding "5px"]
            [
                div [] [text tag];
                div []
                    [
                        text
                            (
                                Printf.sprintf
                                    "%02d:%02d:%02d"
                                    (Time.hour   time zone)
                                    (Time.minute time zone)
                                    (Time.second time zone)
                            )
                    ];
            ]
    in
    div
        [
            style "font-size" "20px";
            style "display" "flex";
            style "flex-direction" "row";
        ]
        [
            time "your zone" state.time state.zone;
            time "utc" state.time Time.Zone.utc;
        ]
