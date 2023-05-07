open Fmlib_browser
open Fmlib_std

type undo = [
    | `Draw
    | `Resize of int * int  (* circle id, old radius *)
]

type redo = [
    | `Redraw of int * int  (* x, y *)
    | `Resize of int * int  (* circle id, new radius *)
]


type circle = {
    center_x: int;
    center_y: int;
    radius: int;
    selected: bool;
}


type state = {
    resizing: (int * int) option;   (* circle id, old radius *)
    undo: undo list;
    redo: redo list;
    circles: circle array;
}

type msg =
    | Ignore
    | Enter of int
    | Leave of int
    | Draw of int * int         (* x, y position of new circle *)
    | Try_resize of int         (* new radius *)
    | Resize
    | Resizing of int * int     (* circle id, old radius*)
    | Undo
    | Redo


let default_circle (center_x: int) (center_y: int): circle =
    {center_x; center_y; selected = false; radius = 20}


let init: state = {
    resizing = None;
    undo = [];
    redo = [];
    circles = [||];
}


let update (state: state): msg -> state =
    function
    | Ignore ->
        state

    | Enter id ->
        let circle = state.circles.(id) in
        {state with
         circles =
             Array.replace
                 id
                 {circle with selected = true}
                 state.circles
        }

    | Leave id ->
        let circle = state.circles.(id) in
        {state with
         circles =
             Array.replace
                 id
                 {circle with selected = false}
                 state.circles
        }

    | Draw (center_x, center_y) ->
        {state with
         circles = Array.push (default_circle center_x center_y) state.circles;
         undo    = `Draw :: state.undo;
         redo    = [];
        }

    | Try_resize radius ->
        begin match state.resizing with
        | None ->
            assert false (* cannot happen *)
        | Some (id, _) ->
            let circle = state.circles.(id) in
            {state with
             circles = Array.replace id {circle with radius} state.circles;
            }
        end

    | Resize ->
        begin match state.resizing with
        | None ->
            assert false (* cannot happen *)
        | Some (i, old_radius) ->
            {state with
             resizing = None;
             undo = `Resize (i, old_radius) :: state.undo;
             redo = [];
            }
        end

    | Resizing (id, old_radius) ->
        begin match state.resizing with
        | None ->
            {state with resizing = Some (id, old_radius)}
        | Some _ ->
            state   (* Ignore event, already resizing another circle *)
        end

    | Undo ->
        begin
            match state.undo with
            | `Draw :: undo ->
                assert (state.resizing = None);
                assert (0 < Array.length state.circles);
                let circle = Array.last state.circles in
                {state with
                 circles = Array.remove_last state.circles;
                 undo;
                 redo = `Redraw (circle.center_x, circle.center_y) :: state.redo;
                }
            | `Resize (id, radius) :: undo ->
                assert (state.resizing = None);
                assert (id < Array.length state.circles);
                let circle = state.circles.(id) in
                {state with
                 circles = Array.replace id {circle with radius} state.circles;
                 undo;
                 redo = `Resize (id, circle.radius) :: state.redo;
                }
            | [] ->
                assert false (* cannot happen *)
        end

    | Redo ->
        begin
            match state.redo with
            | `Redraw (center_x, center_y) :: redo ->
                {state with
                 circles =
                     Array.push (default_circle center_x center_y) state.circles;
                 undo = `Draw :: state.undo;
                 redo
                }

            | `Resize (id, radius) :: redo ->
                assert (state.resizing = None);
                assert (id < Array.length state.circles);
                let circle = state.circles.(id) in
                {state with
                 circles = Array.replace id {circle with radius} state.circles;
                 undo = `Resize (id, circle.radius) :: state.undo;
                 redo;
                }

            | [] ->
                assert false (* cannot happen *)
        end



let view_circle (i: int) (circle: circle): msg Html.t =
    let open Html in
    let open Attribute in
    let int_attr key i =
        attribute key (string_of_int i)
    in
    svg_node
        "circle"
        [ int_attr "cx" circle.center_x
        ; int_attr "cy" circle.center_y
        ; int_attr "r"  circle.radius
        ; on "mouseenter" Decoder.(return (Enter i))
        ; on "mouseleave" Decoder.(return (Leave i))
        ; handler
                "click"
                Event_flag.stop (* stop, otherwise bubbles to svg which makes a
                                   new circle *)
                Event_flag.no_prevent
                Decoder.(return (Resizing (i, circle.radius)))
        ; style "stroke" "black"
        ; style "stroke-width" "2"
        ; style
                "fill"
                (if circle.selected then "blue" else "gray")
        ] []


let view_circles (state: state): msg Html.t list =
    List.rev
        (Array.foldi_left
             (fun lst i circle -> view_circle i circle :: lst)
             []
             state.circles)




let view_resize (state: state): msg Html.t list =
    let open Html in
    let open Attribute in
    match state.resizing with
    | None ->
        []
    | Some (i, _) ->
        let circle = state.circles.(i) in
        [ input [
              attribute "type" "range"
            ; attribute "min" "10"
            ; attribute "max" "100"
            ; attribute "step" "1"
            ; attribute "display" "block"
            ; on_input
                  (fun value ->
                       match int_of_string_opt value with
                       | None ->
                           assert false (* cannot happen *)
                       | Some radius ->
                           Try_resize radius)
            ; value (string_of_int circle.radius)
          ] []
        ; button [on_click Resize] [text "Ok"]
        ]


let view (state: state): msg Html.t =
    let open Html in
    let open Attribute in
    let disabled b =
        property "disabled" Value.(bool b)
    in
    let disabled_undo =
        disabled (state.resizing <> None || state.undo = [])
    and disabled_redo =
        disabled (state.resizing <> None || state.redo = [])
    in
    div [] (
        h1 [] [text "Draw Circles"]
        :: div [] [
            button [on_click Undo; disabled_undo] [text "Undo"]
          ; button [on_click Redo; disabled_redo] [text "Redo"]

        ]
        :: svg_node
            "svg"
            [ attribute "width" "600"
            ; attribute "height" "400"
            ; attribute "display" "block"
            ; border_style "solid"
            ; on "click" (
                  let open Decoder in
                  let* x = field "offsetX" int in
                  let* y = field "offsetY" int in
                  let* i = field "button"  int in
                  if i <> 2 && state.resizing = None then
                      return (Draw (x, y))
                  else
                      return Ignore
              )
            ]
            (view_circles state)
        :: view_resize state
    )


let _ =
    sandbox init view update
