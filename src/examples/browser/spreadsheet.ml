(*

    cell: B12       = A1 + A2


        A       B       C       D       E       ...

    1   0               100

    2                           2000

    3

    4


    The view consists of the header line and a reference element representing
    the sheet.

    The sheet is a header line and [height] cell rows.

    Each cell row has a label and the actual cell. The actual cell is a
    reference element because its value can change dynamically.


*)








(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Basics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


open Fmlib_browser



let cell_width        = "5em"


let column_name (x: int): string =
    assert (x < 26);
    Printf.sprintf "%c" (Char.chr (Char.code 'A' + x))


let row_name (y: int): string =
    string_of_int (y + 1)








(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Html
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


module Html =
struct
    include Html

    let td attrs kids =
        node "td" attrs kids

    let th attrs kids =
        node "th" attrs kids

    let tr attrs kids =
        node "tr" attrs kids

    let table attrs kids =
        node "table" attrs kids
end





(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Cell Id
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)




module Id =
struct
    type t = int * int

    let compare (a: t) (b: t): int =
        compare a b

    let make x y =
        (x, y)

    let x ((x, _): t): int =
        x

    let y ((_, y): t): int =
        y

    let name ((x,y): t): string =
        Printf.sprintf "%s%s" (column_name x) (row_name y)


    let up ((x, y) as id: t): t =
        if y = 0 then
            id
        else
            (x, y - 1)

    let down (height: int) ((x, y) as id: t): t =
        if y + 1 = height then
            id
        else
            (x, y + 1)

    let left ((x, y) as id: t): t =
        if x = 0 then
            id
        else
            (x - 1, y)

    let right (width: int) ((x, y) as id: t): t =
        if x + 1 = width then
            id
        else
            (x + 1, y)
end







(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)



module Parser =
struct
    open Fmlib_parse

    module Final =
    struct
        type t = (float array -> float)
    end

    module State =
    struct
        type t = Id.t * Id.t array

        let id s = fst s

        let args s = snd s

        let push_arg id (id0, args) =
            (id0, Fmlib_std.Array.push id args)
    end


    type exp = Final.t


    include Character.Make (State) (Final) (String)


    let get_id: Id.t t =
        map State.id get


    let get_args: Id.t array t =
        map State.args get


    let cell_index (cell_id: Id.t): int t =
        let* args = map State.args get in
        match
            Fmlib_std.Array.find (fun arg_id -> cell_id = arg_id) args
        with
        | None ->
            let* _ = update (State.push_arg cell_id) in
            let* args2 = map State.args get in
            assert Array.(length args + 1 = length args2);
            return (Array.length args)

        | Some idx ->
            return idx



    let whitespace: int t =
        skip_zero_or_more (char ' ' </> char '\n')


    let lexeme (p: 'a t): 'a t =
        let* a = p in
        let* _ = whitespace in
        return a


    let binary_operator: (char * (float -> float -> float)) t =
        let mapc op c = (c, op)
        in
        map (mapc ( +. )) (char '+')
        </>
        map (mapc ( -. )) (char '-')
        </>
        map (mapc ( *. )) (char '*')
        |>
        lexeme


    let int_number: int t =
        one_or_more_fold_left
            return
            (fun v d -> 10 * v + d |> return)
            digit


    let fraction: float t =
        one_or_more_fold_left
            (fun d -> return (0.01, 0.1 *. d))
            (fun (base, v) d -> (0.1 *. base, v +. base *. d) |> return)
            (map float_of_int digit)
        |> map snd


    let number_dot_number: float t =
        let* predot =
            map float_of_int int_number
        in
        let* postdot =
            (
                let* _ = char '.' in
                fraction
            ) |> optional
        in
        match postdot with
        | None ->
            return predot
        | Some frac ->
            return (predot +. frac)


    let number: exp t =
        lexeme (map (fun n _ -> n) number_dot_number)


    let cell: exp t =
        (
            let* x =
                (charp (fun c -> 'A' <= c && c <= 'Z') "Letter [A-Z]"
                 |> map (fun c -> Char.code c - Char.code 'A'))
                </>
                (charp (fun c -> 'a' <= c && c <= 'z') "Letter [a-z]"
                 |> map (fun c -> Char.code c - Char.code 'a'))
            in
            let* y = int_number in
            let y = y - 1 in
            let* id = get_id in
            if x < Id.x id && 0 <= y && y < Id.y id then
                let cell_id = Id.make x y in
                let* i = cell_index cell_id in
                return (fun args -> args.(i))
            else
                fail "Illegal cell"
        )
        |> lexeme



    let rec expr (): exp t =
        let is_left (op1, _) (op2, _): bool t =
            return (op1 = '*' || op2 <> '*')

        and make_unary _ _ : exp t =
            assert false

        and make_binary exp1 (_, op) exp2: exp t =
            return (fun args -> op (exp1 args) (exp2 args))

        and primary (): exp t =
            number
            </>
            cell
            </>
            parenthesized
                (fun _ a _ -> return a)
                (char '(' |> map (fun _ -> ')') |> lexeme)
                expr
                (fun _ -> char ')' |> lexeme)
        in
        operator_expression
            (primary ())
            None
            binary_operator
            is_left
            make_unary
            make_binary


    let expr_opt: exp option t =
        expr () |> optional


    let formula: exp t =
        let* _ = whitespace
        in
        expr ()
        </>
        ( let* _ = char '=' |> lexeme
          in expr ())
        </>
        return (fun _ -> 0.)



    let parse
            (id: Id.t)
            (input: string)
        : (Id.t array * Final.t, string) result
        =
        let p = make (id, [||]) formula
        in
        let p = Parser.run_on_string input p
        in
        if Parser.has_succeeded p then
            let args = Parser.state p |> State.args
            and formula = Parser.final p
            in
            Ok (args, formula)
        else if Parser.has_failed_semantic p then
            Error (Parser.failed_semantic p)
        else
            Error "Syntax"
end







(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Message
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


type msg =
    | Nothing
    | Up
    | Down
    | Left
    | Right
    | Goto  of Id.t
    | Input of string
    | Edit
    | Enter
    | Escape


let input_msg str = Input str








(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Cell
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)




module Cell =
struct
    type status =
        | Idle
        | Sorting
        | Sorted


    type t = {
        id: Id.t;
        mutable edit: string;                     (* string entered *)
        mutable value: float;
        mutable users: Id.t array;
        mutable suppliers: Id.t array;
        mutable formula: float array -> float;
        mutable status: status;
        mutable selected: bool;
    }


    let id c        = c.id
    let name c      = Id.name c.id
    let users c     = c.users
    let suppliers c = c.suppliers
    let formula c   = c.formula
    let value c     = c.value
    let selected c  = c.selected
    let edit c      = c.edit


    let make (id: Id.t): t =
        {
            id;
            edit = "";
            value = 0.;
            users = [||];
            suppliers = [||];
            formula = (fun _ -> 0.);
            status  = Idle;
            selected = false;
        }


    let display (c: t): string =
        if c.edit = "" then
            ""
        else
            Printf.sprintf "%g" c.value


    let view (c: t): msg Html.t =
        let open Html in
        let open Attribute in
        div
            [ on_click (Goto c.id)
            ; height "1.2em"
            ; width cell_width
            ; background_color (if c.selected then "yellow" else "white");
            ]
            [display c |> text]


    let command (c: t): msg Command.t =
        Command.set_reference
            (Id.name c.id)
            (view c)


    let count_users (c: t): int =
        Array.length c.users


    let user (i: int) (c: t): Id.t =
        assert (i < count_users c);
        c.users.(i)



    let is_idle (c: t): bool =
        c.status = Idle

    let is_sorting (c: t): bool =
        c.status = Sorting

    let is_sorted (c: t): bool =
        c.status = Sorted

    let set_idle (c: t): unit =
        assert (is_sorted c);
        c.status <- Idle

    let set_sorting (c: t): unit =
        assert (is_idle c);
        c.status <- Sorting

    let set_sorted (c: t): unit =
        assert (is_sorting c);
        c.status <- Sorted


    let select (c: t): unit =
        c.selected <- true


    let unselect (c: t): unit =
        c.selected <- false


    let remove_user (user: Id.t) (c: t): unit =
        let open Fmlib_std
        in
        match
            Array.find
                (fun u -> u = user)
                c.users
        with
        | None ->
            assert false (* Illegal call *)
        | Some idx ->
            c.users <-
                Array.remove idx c.users


    let add_user (user: Id.t) (c: t): unit =
        assert (
            Array.for_all
                (fun u -> u <> user)
                c.users
        );
        c.users <-
            Fmlib_std.Array.push user c.users



    let set_suppliers (suppliers: Id.t array) (c: t): unit =
        c.suppliers <- suppliers



    let set_formula
            (edit: string)
            (formula: float array -> float)
            (c: t)
        : unit
        =
        c.edit    <- edit;
        c.formula <- formula




    let set_value (args: float array) (c: t): msg Command.t =
        assert Array.(length args = length c.suppliers);
        c.value <- c.formula args;
        command c
end







(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Sheet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let view_sheet (ncols: int) (nrows: int): msg Html.t =
    let open Html in
    let open Attribute
    in
    let header_style = background_color "lightgrey"
    in
    let header_row: msg Html.t =
        tr [] (
            td [] []
            ::
            List.init
                ncols
                (fun x ->
                     th
                         [ header_style ]
                         [ text (column_name x) ] )
        )

    and header_column y: msg Html.t =
        th  [ header_style ] [text (row_name y)]
    in
    let cell y x =
        td
            []
            [ reference (Id.make x y |> Id.name)]
    in
    let row y: msg Html.t =
        tr
            []
            ( header_column y
              ::
              List.init
                  ncols
                  (cell y)
            )
    in
    table
        []
        (header_row :: List.init nrows row)




let make_sheet
        (width: int)
        (height: int)
    : Cell.t array array * msg Command.t
    =
    let cells = Array.init height (fun y ->
        Array.init width (fun x ->
            let cell = Cell.make (Id.make x y) in
            if x = 0 && y = 0 then
                Cell.select cell;
            cell
        ))
    in
    let cmd =
        Command.( batch (
            set_reference "sheet" (view_sheet width height)
            ::
            List.init height (fun y ->
                batch (
                    List.init width (fun x -> Cell.command cells.(y).(x))
                )
            )
        ))
    in
    cells, cmd







(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Model
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

type model = {
    width:  int;
    height: int;
    cells:  Cell.t array array;
    input:  string;
    error:  string;
    select: Id.t;
}


let get_cell (id: Id.t) (s: model): Cell.t =
    let x = Id.x id
    and y = Id.y id in
    assert (y < Array.length s.cells);
    let row = s.cells.(y) in
    assert (x < Array.length row);
    row.(x)


let is_valid_cell (id: Id.t) (s: model): bool =
    let x = Id.x id
    and y = Id.y id in
    0 <= x && x < s.width
    &&
    0 <= y && y <= s.height






(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              View
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let view_flex (dir: string) (lst: msg Html.t list): msg Html.t =
    let open Html in
    let open Attribute in
    div
        [style "display" "flex"; style "flex-direction" dir]
        lst


let view (s: model): msg Html.t =
    let open Html in
    let open Attribute in
    let open Printf in
    div [style "max-height" "100%"] [
        div
            [
                style "display" "flex";
                style "flex-direction" "row";
            ]
            [
                sprintf "cell: %s  content: " (s.select |> Id.name) |> text;
                input
                    [ attribute "id" "input";
                      attribute "type" "text";
                      value s.input;
                      on_input input_msg;
                      handler "keydown"
                          Event_flag.stop
                          Event_flag.no_prevent
                          Decoder.(
                              map
                                  (fun key ->
                                       if key = "Escape" then
                                           Escape
                                       else if key = "Enter" then
                                           Enter
                                       else
                                           Nothing)
                                  (field "key" string)
                          )
                    ]
                    [];
                div [background_color "red"] [
                    text s.error
                ]
            ]
        ;
        div [ style "width" "90vw"
            ; style "height" "90vh"
            ; style "overflow" "auto"]
            [reference "sheet"]
    ]





(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              Update
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)


let select (id: Id.t) (with_focus: bool) (s: model): model * msg Command.t =
    if id = s.select then
        if with_focus then
            s, Command.focus "input"
        else
            s, Command.none
    else
        let cell_old = get_cell s.select s
        and cell_new = get_cell id s
        in
        Cell.unselect cell_old;
        Cell.select cell_new;
        assert (Cell.selected cell_new);
        assert (not (Cell.selected cell_old));
        {s with select = id; input = Cell.edit cell_new}
        ,
        Command.batch [
            Cell.command cell_old
            ;
            Cell.command cell_new
            ;
            Command.blur "input"
        ]




let string_of_list (f: 'a -> string) (lst: 'a list): string =
    "["
    ^
    String.concat
        ","
        (List.map f lst)
    ^
    "]"


let sort_users (s: model): Cell.t list =
    let rec sort stack sorted =
        match stack with
        | [] ->

            sorted

        | (top, n) :: rest ->

            assert (Cell.is_sorting top);
            if n = Cell.count_users top then
                begin
                    Cell.set_sorted top;
                    sort rest (top :: sorted)
                end
            else
                begin
                    let user =
                        get_cell (Cell.user n top) s
                    in
                    if Cell.is_sorted user then
                        sort ((top, n + 1) :: rest) sorted
                    else
                        begin
                            assert (Cell.is_idle user);
                            Cell.set_sorting user;
                            sort
                                ((user, 0) :: (top, n + 1) :: rest)
                                sorted
                        end
                end
    in
    let cell = get_cell s.select s in
    Cell.set_sorting cell;
    sort [cell, 0] []





let make_idle (sorted: Cell.t list): unit =
    List.iter
        Cell.set_idle
        sorted



let insert_suppliers (suppliers: Id.t array) (s: model): unit =
    (* Insert suppliers into the selected cell

       - Remove old suppliers and remove selected cell from the users of the
         suppliers.

       - Add new suppliers and add the current cell to the users of the
         new suppliers.
     *)
    assert (
        Array.for_all
            (fun supplier ->
                 is_valid_cell supplier s)
            suppliers
    );
    let cell = get_cell s.select s in

    (* Remove old suppliers *)
    Array.iter
        (fun supplier ->
             Cell.remove_user s.select (get_cell supplier s)
        )
        (Cell.suppliers cell);

    (* Add new suppliers *)
    Cell.set_suppliers suppliers cell;
    Array.iter
        (fun supplier ->
             Cell.(
                 add_user
                     (id cell)
                     (get_cell supplier s)
             )
        )
        suppliers;
    Cell.set_suppliers suppliers cell




let update_sorted (sorted: Cell.t list) (s: model): msg Command.t =
    let cmds =
        List.fold_left
            (fun cmds cell ->
                 let args =
                     Array.map
                         (fun id -> Cell.value (get_cell id s))
                         (Cell.suppliers cell)
                 in
                 Cell.set_value args cell :: cmds)
            []
            sorted
    in
    make_idle sorted;
    Command.batch (Command.blur "input" :: cmds)




let update_selected (s: model): model * msg Command.t =
    (* Update the selected cell with the editor input string *)
    let s =
        {s with input = String.trim s.input}
    in

    match
        Parser.parse
            (Id.make s.width s.height)
            s.input
    with
    | Error error ->
        {s with error},
        Command.none

    | Ok (suppliers, formula) ->
        let sorted = sort_users s in
        let has_cycle =
            List.exists
                (fun user ->
                     Array.exists
                         (fun supplier -> Cell.id user = supplier)
                         suppliers
                )
                sorted
        in
        if has_cycle then
            begin
                make_idle sorted;
                {s with error = "Cycle"}, Command.none
            end
        else
            begin
                insert_suppliers suppliers s;
                Cell.set_formula s.input formula (get_cell s.select s);
                {s with error = ""},
                update_sorted sorted s
            end




let update s =
    function
    | Nothing ->
        s, Command.none

    | Up ->
        select (Id.up s.select) false s

    | Down ->
        select (Id.down s.height s.select) false s

    | Left ->
        select (Id.left s.select) false s

    | Right ->
        select (Id.right s.width s.select) false s

    | Goto id ->
        select id true s

    | Input input ->
        {s with input}, Command.none

    | Edit ->
        s, Command.focus "input"

    | Enter ->
        update_selected s

    | Escape ->
        {s with
         input = get_cell s.select s |> Cell.edit;
         error = "";}
        ,
        Command.blur "input"












(*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)



let init (width: int) (height: int): model * msg Command.t =
    assert (0 < height);
    assert (0 < width);
    assert (width <= 26);

    let cells, cmd = make_sheet width height in
    {
        width;
        height;
        cells;
        input = "";
        error = "";
        select = Id.make 0 0;
    }
    ,
    cmd



let _ =
    let s, cmd = init 26 99 in
    basic_application
        s
        cmd
        (fun s -> view s, "Sheet")
        (fun _ ->
             Subscription.on_keydown
                 (fun str ->
                      if str = "ArrowUp" then
                          Up
                      else if str = "ArrowDown" then
                          Down
                      else if str = "ArrowLeft" then
                          Left
                      else if str = "ArrowRight" then
                          Right
                      else if str = "Enter" then
                          Edit
                      else
                          Nothing
                 )
        )
        update
