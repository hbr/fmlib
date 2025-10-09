open Fmlib_browser

type date = {
    year: int;
    month: int;
    day: int;
}


type kind =
    | Oneway
    | Return

type state =
    | Booking of kind * date option * date option
    | Booked of date * date option


type msg =
    | Select_oneway
    | Select_return
    | Flight_date of date option
    | Return_date of date option
    | Book
    | Reset


let init: state =
    Booking (Oneway, None, None)



let update (state: state) (msg: msg): state =
    match msg, state with
    | Reset, _ ->
        init

    | Select_oneway, Booking (_, flight, _) ->
        Booking (Oneway, flight, None)

    | Select_return, Booking (_, flight, return) ->
        Booking (Return, flight, return)

    | Book, Booking (_, Some flight, return) ->
        Booked (flight, return)

    | Flight_date flight, Booking (kind, _, return) ->
        Booking (kind, flight, return)

    | Return_date return, Booking (kind, flight, _) ->
        Booking (kind, flight, return)

    | _ ->
        assert false (* cannot happen *)








(* Helper functions for view functions
 * ============================================================
 *)


let compare (d1: date) (d2: date): int =
    let cmp_year = compare d1.year d2.year in
    if cmp_year <> 0 then
        cmp_year
    else
        let cmp_month = compare d1.month d2.month in
        if cmp_month <> 0 then
            cmp_month
        else
            compare d1.day d2.day


let string_of_date (date: date): string =
    let open Printf in
    (if date.year < 1000 then
         sprintf "%04d" date.year
     else
         string_of_int date.year
    )
    ^ "-" ^ sprintf "%02d" date.month
    ^ "-" ^ sprintf "%02d" date.day


let string_of_date_opt: date option -> string =
    function
    | None ->
        ""
    | Some date ->
        string_of_date date


let date_of_string (str: string): date option =
    match String.split_on_char '-' str with
    | [""] ->
        None
    | [year; month; day] ->
        let open Fmlib_std.Option in
        let* year  = int_of_string_opt year in
        let* month = int_of_string_opt month in
        let* day   = int_of_string_opt day in
        return {year; month; day}
    | _ ->
        assert false (* cannot happen *)



let decode_date: date option Decoder.t =
    let open Decoder in
    field "target" (field "value" (map date_of_string string))



let decode_selection: msg Decoder.t =
    let open Decoder in
    field
        "target"
        (field
             "selectedIndex"
             (map
                  (fun i -> if i = 0 then Select_oneway else Select_return)
                  int))






(* View functions
 * ============================================================
 *)

let view_date_opt
        (date: date option)
        (txt: string)
        (decode: msg Decoder.t)
    : msg Html.t
    =
    let open Html in
    let open Attribute in
    p [] [
        Html.label [] [
            input [
                attribute "type" "date"
              ; value (string_of_date_opt date)
              ; on "input" decode
            ] []
          ; text txt
        ]
    ]


let view_booking
        (booking_kind: kind)
        (flight: date option)
        (return: date option)
    : msg Html.t
    =
    let open Html in
    let open Attribute in
    let date d lab dec =
        view_date_opt d lab dec
    and decode_flight =
        Decoder.map (fun date -> Flight_date date) decode_date
    and decode_return =
        Decoder.map (fun date -> Return_date date) decode_date
    in
    let dates =
        match booking_kind with
        | Oneway ->
            [date flight "" decode_flight]
        | Return ->
            [date flight " flight" decode_flight
            ; date return " return flight" decode_return]
    and book_button =
        match booking_kind, flight, return with
        | Oneway, Some _, _ ->
            [p [] [button [on_click Book] [text "Book"]]]
        | Return, Some d1, Some d2 ->
            if compare d1 d2 < 0 then
                [p [] [button [on_click Book] [text "Book"]]]
            else
                [p
                     [color "red"]
                     [text "return flight has to be after the flight"]
                ]
        | _ ->
            []
    and one_way =
        match booking_kind with  Oneway -> true | Return -> false
    in
    div []
        (
            h1 [] [text "Book your flight"]
            ::
            select
                [on "change" decode_selection]
                [ option
                    [property "selected" Value.(bool one_way)]
                    [text "One way"]
                ; option
                    [property "selected" Value.(bool (not one_way))]
                    [text "Return"]
                ]
            ::
            (dates @ book_button)
        )



let view_booked
        (flight: date)
        (return: date option)
    : msg Html.t
    =
    let open Html in
    let open Attribute in
    let date_element txt date =
        p [] [
            Html.label [] [
                input [ type_ "date"
                      ; readonly true
                      ; value (string_of_date date)
                      ][]
              ; text txt
            ]
        ]
    in
    let reset =
        [p [] [button [on_click Reset] [text "New booking"]]]
    in
    let lst =
        match return with
        | None ->
            reset
        | Some return ->
            date_element " return flight" return :: reset
    in

    div [] (
        h1 [] [text "Booking Confirmation"]
        ::
        date_element " flight" flight
        ::
        lst
    )



let view: state -> msg Html.t =
    function
    | Booking (kind, flight, return) ->
        view_booking kind flight return
    | Booked (flight, return) ->
        view_booked flight return



let _ =
    sandbox init view update
