
(* This module handles all calls to javascript

*)

module Base = Fmlib_js.Base
module Event_target = Fmlib_js.Event_target
module Timer = Fmlib_js.Timer
module Element = Fmlib_js.Dom.Element
module Dom = Fmlib_js.Dom
module Node = Dom.Node
module Style = Dom.Style
module Document = Dom.Document
module Window = Dom.Window
module Result = Fmlib_std.Result


(* Basic definitions
   ============================================================
*)


module String_map = Fmlib_std.Btree.Map (String)


type element             = Element.t * Handler.EventHs.t

type node                = Node.t * element option

type 'msg dom            = ('msg, node) Vdom.t1

type 'msg ref_dom        = Element.t * ('msg, node) Vdom.t1 option ref

type 'msg dom_operations = ('msg, node) Vdom.operations


let document (): Document.t = Window.(get () |> document)





(*  View and Upate Operations
    ============================================================
*)

type ('state, 'msg) init   = unit -> ('state * 'msg Command.t) Base.Decode.t

type ('state, 'msg) view   = 'state -> 'msg Vdom.t * (unit -> unit)

type ('state, 'msg) update = 'state -> 'msg -> 'state * 'msg Command.t

type ('state, 'msg) subscriptions = 'state -> 'msg Subscription.t




let make_init1 (s: 'state) (c: 'msg Command.t): ('state, 'msg) init =
    fun () -> (Base.Decode.return (s, c))


let make_view1 (v: 'state -> 'msg Vdom.t): ('state, 'msg) view =
    fun s -> v s, (fun () -> ())


let make_view2 (v: 'state -> 'msg Vdom.t * string): ('state, 'msg) view =
    fun s ->
    let vdom, title = v s
    in
    vdom,
    (fun () ->
         Document.set_title title (document ()))


let make_update1 (u: 'state -> 'msg -> 'state): ('state, 'msg) update =
    fun s m -> u s m, Command.none







(*  State
    ============================================================
*)


module State:
sig
    type ('state, 'msg) data

    val view: ('s, 'm) data -> 'm Vdom.t * (unit -> unit)

    val subscriptions: ('s, 'm) data -> 'm Subscriptions.t

    val root_node: ('s, 'm) data -> Node.t

    val is_dom_dirty: ('s, 'm) data -> bool

    val clean_dom: ('s, 'm) data -> unit

    val make:
        's
        -> Element.t
        -> ('s, 'm) view
        -> ('s, 'm) update
        -> ('s, 'm) subscriptions
        -> (Base.Value.t -> unit)
        -> ('s, 'm) data

    val dom_ops: ('s, 'm) data -> 'm dom_operations

    val dispatch: ('s, 'm) data -> 'm -> unit

    val update_subscriptions: ('s, 'm) data -> unit

    val execute_command:
        ('s, 'm) data
        -> 'm Command.t
        -> unit
end
=
struct
    type ('state, 'msg) data =
        {
            mutable state: 'state;
            mutable dirty_dom: bool;
            mutable dirty_sub: bool;
            mutable dom:   'msg dom option;
            mutable subs:  'msg Subscriptions.t;
            mutable ref_doms: 'msg ref_dom String_map.t;
            root:          Element.t;
            view:          ('state, 'msg) view;
            update:        ('state, 'msg) update;
            subscriptions: ('state, 'msg) subscriptions;
            post_js:       Base.Value.t -> unit;
        }


    let view data =
        data.view data.state


    let subscriptions data =
        data.subs


    let root_node data = Element.node data.root


    let is_dom_dirty data = data.dirty_dom


    let clean_dom data: unit =
        data.dirty_dom <- false


    let clean_sub data: unit =
        data.dirty_sub <- false



    let ref_dom (name: string) (data: ('s, 'm) data): 'm ref_dom =
        match
            String_map.find_opt name data.ref_doms
        with
        | None ->
            let root = Document.create_element "span" (document ()) in
            let dom  = root, ref None in
            data.ref_doms <- String_map.add name dom data.ref_doms;
            dom

        | Some dom ->
            dom




    (*  Initial Data
        ============================================================
    *)


    let wrap_state_fun (str: string) (f: 's -> 'a) (state: 's): 'a =
        Assert_failure.attempt
            ("Exception in '" ^ str ^ "'")
            (fun () -> f state)
            (fun () -> ())


    let wrap_view (view: 's -> 'a) (state: 's): 'a =
        wrap_state_fun "view" view state


    let wrap_subscription (view: 's -> 'a) (state: 's): 'a =
        wrap_state_fun "subscription" view state


    let wrap_update (update: 's -> 'm -> 'a) (state: 's) (message: 'm): 'a =
        Assert_failure.attempt
            "Exception in 'update'"
            (fun () -> update state message)
            (fun () -> ())


    let make
            (state: 'state)
            (root: Element.t)
            (view: ('state, 'msg) view)
            (update: ('state, 'msg) update)
            (subscriptions: ('state, 'msg) subscriptions)
            (post_js: Base.Value.t -> unit)
        : ('state, 'msg) data
        =
        let view          = wrap_view view
        and update        = wrap_update update
        and subscriptions = wrap_subscription subscriptions
        in
        {
            state;
            dirty_dom = true;
            dirty_sub = true;
            root;
            ref_doms = String_map.empty;
            dom   = None;
            subs  = Subscriptions.empty ();
            view;
            update;
            subscriptions;
            post_js;
        }





    (* Dom operations
       ============================================================

       Accessing the real dom.

       Operations are needed for the module [Vdom].
    *)


    let dom_operations
            (dispatch: 'msg -> unit)
            (reference_root: string -> Element.t)
        : 'msg dom_operations
        =
        let get_both:
            node -> element
            =
            function
            | (_, Some el) ->
                el
            | _ ->
                assert false (* Illegal call *)
        in
        let get_element:
            Node.t * element option -> Element.t
            =
            function
            | (_, Some (el, _)) ->
                el
            | _ ->
                assert false (* Illegal call *)
        in
        {
            make_text =
                (fun s -> Document.create_text_node s (document ()), None);

            make_element =
                (fun tag (lst: node list) ->
                     let open Document in
                     let open Element in
                     let open Node in
                     let doc = document () in
                     let el  = create_element tag doc in
                     List.iter
                         (fun (child, _) ->
                              append child (node el))
                         lst;
                     node el, Some (el, Handler.EventHs.empty ()));

            make_element_ns =
                (fun namespace tag (lst: node list) ->
                     let open Document in
                     let open Element in
                     let open Node in
                     let doc = document () in
                     let el  = create_element_ns namespace tag doc in
                     List.iter
                         (fun (child, _) ->
                              append child (node el))
                         lst;
                     node el, Some (el, Handler.EventHs.empty ()));

            get_reference =
                (fun id ->
                     let el = reference_root id
                     in
                     Element.node el, Some (el, Handler.EventHs.empty()));

            add_child =
                (fun (child, _) (par, _) ->
                     Node.append child par);

            remove_child =
                (fun (child, _) (par, _) ->
                     Node.remove child par);

            remove_children =
                (fun (par, _) ->
                     Node.remove_children par);

            replace_child =
                (fun (old_child, _) (new_child, _) (par, _) ->
                     Node.replace new_child old_child par);

            set_text =
                (fun (node, _) text ->
                     Node.set_node_value text node);


            set_style =
                (fun el key value ->
                     Style.set key value (Element.style (get_element el)));

            set_attribute =
                (fun el key value ->
                     Element.set_attribute key value (get_element el));

            set_property =
                (fun el key value ->
                     Element.set_property key value (get_element el));


            remove_style =
                (fun el key ->
                     Style.remove
                         key
                         (Element.style (get_element el)));


            remove_attribute =
                (fun el key ->
                     Element.remove_attribute key (get_element el));


            remove_property =
                (fun el key ->
                     Element.delete_property key (get_element el));


            set_handlers =
                (fun el dict ->
                     let el, reals = get_both el in
                     let target = Node.event_target (Element.node el) in
                     Handler.EventHs.set target dispatch dict reals
                );

            update_handlers =
                (fun el dict1 dict2 ->
                     let el, reals = get_both el in
                     let target = Node.event_target (Element.node el) in
                     Handler.EventHs.update target dispatch dict1 dict2 reals);
        }





    (* Dispatching messages to the application
       ============================================================

       - call the update function

            state -> msg -> state * cmd

       - update the subscriptions

            state might have changed, therefore subscriptions might have changed.

       - execute commands

    *)

    let rec dispatch (data: ('state, 'msg) data) (msg: 'msg): unit =
        let state, cmd = data.update data.state msg
        in
        let different = not (state == data.state)
        in
        data.state     <- state;
        data.dirty_dom <- data.dirty_dom || different;
        data.dirty_sub <- data.dirty_sub || different;
        update_subscriptions data;
        execute_command data cmd


    and dispatch_next (data: ('state, 'msg) data) (msg: 'msg): unit =
        ignore ( Timer.set (fun () -> dispatch data msg) 0 )


    and update_subscriptions (data: ('s, 'm) data): unit =
        (* update the subscriptions, i.e. install all necessary handlers. *)
        if data.dirty_sub then
            begin
                clean_sub data;

                data.subs      <-
                    Subscriptions.update
                        (dispatch data)
                        (data.subscriptions data.state)
                        data.subs;
            end


    and dom_ops (data: ('s, 'm) data): 'm dom_operations =
        dom_operations
            (dispatch data)
            (fun name -> ref_dom name data |> fst)



    and set_reference
            (data: ('s, 'm) data)
            (ops:  'm dom_operations)
            (name: string)
            (vd_new: 'm Vdom.t)
        : unit
        =
        let root, vdref = ref_dom name data
        in
        match !vdref with
        | None ->
            let vd = Vdom.make ops vd_new in
            Node.append
                (Vdom.element vd |> fst)
                (Element.node root);
            vdref := Some vd

        | Some vd_old ->
            let vd, created = Vdom.update ops vd_new vd_old in
            if created then
                begin
                    Node.replace
                        (Vdom.element vd     |> fst)
                        (Vdom.element vd_old |> fst)
                        (Element.node root);
                end;
            vdref := Some vd


    and execute_command
            (data: ('s, 'm) data)
            (cmd: 'm Command.t)
        : unit
        =
        Command.execute
            data.post_js
            (dispatch_next data)
            (set_reference data (dom_ops data))
            cmd
end (* State *)










(*  Application
    ============================================================
*)


module Application:
sig
    type ('s, 'm) def = {
        name: string;
        is_element: bool;
        can_send_to_js: bool;
        js_init: bool;
        init: ('s, 'm) init;
        view: ('s, 'm) view;
        update: ('s, 'm) update;
        subscription: ('s, 'm) subscriptions;
    }

    val make_def:
        string
        -> bool
        -> bool
        -> bool
        -> ('s, 'm) init
        -> ('s, 'm) view
        -> ('s, 'm) update
        -> ('s, 'm) subscriptions
        -> ('s, 'm) def

    val main_js: ('s, 'm) def -> unit

    val main_onload: ('s, 'm) def -> unit
end
=
struct
    type  'a res = ('a, string) result

    type global = {
        mutable is_initialized: bool;
        mutable receive: Base.Value.t -> unit;
    }

    type ('s, 'm) def = {
        name: string;
        is_element: bool;
        can_send_to_js: bool;
        js_init: bool;
        init: ('s, 'm) init;
        view: ('s, 'm) view;
        update: ('s, 'm) update;
        subscription: ('s, 'm) subscriptions;
    }


    type ('s, 'm) t = {
        data: ('s, 'm) State.data;
        ops:  'm dom_operations;
        mutable dom: 'm dom;
    }


    let global: global = {
        is_initialized =
            false;
        receive =
            (fun _ ->
                 Base.Main.log_string "Application not yet initialised")
    }


    let receive (g: global) (v: Base.Value.t): unit =
        g.receive v



    let set_initialized (): unit res =
        if global.is_initialized then
            Error "Only one application can be initialized"
        else
            begin
                global.is_initialized <- true;
                Ok ()
            end


    let make_def
            name
            is_element
            can_send_to_js
            js_init
            init
            view
            update
            subscription
        : ('s, 'm) def
        =  { name;
             is_element;
             can_send_to_js;
             js_init;
             init;
             view;
             update;
             subscription }



    let put_below_root (data: ('state, 'msg) State.data) (dom: 'msg dom): unit =
        let root_node = State.root_node data in
        Node.remove_children root_node;
        Node.append (Vdom.element dom |> fst) root_node


    let update_dom (s: ('s, 'm) t): unit =
        if State.is_dom_dirty s.data then
            begin
                State.clean_dom s.data;

                let vdom, set_title = State.view s.data in
                set_title ();

                let dom, created = Vdom.update s.ops vdom s.dom
                in
                s.dom <- dom;
                if created then
                    put_below_root s.data dom
            end


    let on_next_animation (f: float -> unit): unit =
        Window.(on_next_animation f (get ()))


    let rec animate (s: ('s, 'm) t): float -> unit =
        fun time ->
        Subscriptions.on_animation
            time
            (State.dispatch s.data)
            (State.subscriptions s.data);
        update_dom s;
        on_next_animation (animate s)


    let get_root (is_element: bool) (js_init: Base.Value.t): Element.t res =
        if is_element then
            let open Fmlib_std.Result
            in
            let* id =
                js_init
                |> Base.Decode.(field "element_id" string)
                |> Option.to_result ~none:"No element_id found"
            in
            Dom.Document.find id (document ())
            |> Option.to_result
                ~none:(Printf.sprintf "Cannot find element <%s>" id)

        else
            Ok (Document.body (document ()))



    let get_data
            (js_init: bool)
            (decode: ('s * 'm Command.t) Base.Decode.t)
            (js_obj: Base.Value.t)
        : ('s * 'm Command.t) res
        =
        js_obj
        |> (if js_init then
                Base.Decode.(field "data" decode)
            else
                decode)
        |> Option.to_result ~none:"Cannot decode initialisation data"


    let get_send_to_js
            (can_send_to_js: bool)
            (js_obj: Base.Value.t)
        : (Base.Value.t -> unit) res
        =
        if can_send_to_js then
            js_obj
            |> Base.Decode.(
                map
                    (fun f v -> ignore (f [|v|]))
                    (field "onMessage" _function)
            )
            |> Option.to_result ~none:"Cannot decode onMessage function"
        else
            Ok (fun _ -> ())



    let init
            (def: ('s, 'm) def)
            (js_init: Base.Value.t)
        : unit
        =
        match
            let open Result in
            let* root       = get_root def.is_element js_init in
            let* state, cmd = get_data def.js_init (def.init ()) js_init in
            let* post_js    = get_send_to_js def.can_send_to_js js_init in
            let* _          = set_initialized ()
            in
            let data =
                State.make
                    state
                    root
                    def.view
                    def.update
                    def.subscription
                    post_js
            in
            let ops = State.dom_ops data
            in
            let vdom, set_title = State.view data in
            set_title ();
            global.receive <-
                Subscriptions.on_message
                    (State.dispatch data)
                    (fun _ -> State.subscriptions data);
            let dom =
                Vdom.make ops vdom
            in
            State.clean_dom data;
            put_below_root data dom;
            State.update_subscriptions data;
            State.execute_command data cmd;
            let s = { data; ops; dom } in
            on_next_animation (animate s);
            Ok ()
        with
        | Ok () ->
            ()
        | Error str ->
            Base.Main.log_string str



    let onload (f: _ -> unit): unit =
        Event_target.add "load" f Window.(event_target (get ()))


    let main_js (def: ('s, 'm) def): unit =
        let open Base.Value
        in
        let wrap f v = f v; null
        in
        let init v =
            onload (fun _ -> init def v)
        in
        Base.Main.make_global
            def.name
            (
                _object
                    [| ("init", function1 (wrap init));
                       ("post", function1 (wrap (receive global)))
                    |]
            )

    let main_onload (def: ('s, 'm) def): unit =
        onload (fun _ -> init def Base.Value.null)
end (* Application *)










(* Sandbox application
 * ============================================================
 *)

let sandbox_plus
        (state:  'state)
        (view:   'state -> 'msg Vdom.t)
        (sub:    'state -> 'msg Subscription.t)
        (update: 'state -> 'msg -> 'state)
    : unit
    =
    let open Application in
    main_onload
        (make_def
            "????"
            false
            false
            false
            (make_init1 state Command.none)
            (make_view1 view)
            (make_update1 update)
            sub)



let sandbox
        (state: 'state)
        (view: 'state -> 'msg Vdom.t)
        (update: 'state -> 'msg -> 'state)
    : unit
    =
    sandbox_plus state view (fun _ -> Subscription.none) update









(* Element application
 * ============================================================
 *)






let element
        (name: string)
        (decode: ('s * 'm Command.t) Base.Decode.t)
        (view:   's -> 'm Vdom.t)
        (sub:    's -> 'm Subscription.t)
        (update: ('s, 'm) update)
    : unit
    =
    let open Application in
    main_js
        (make_def
            name
            true
            true
            true
            (fun () -> decode)
            (make_view1 view)
            update
            sub)







(* Single Page Application
 * ============================================================
 *)



let application
        (name: string)
        (init: Url.t -> 'm Navigation.key -> ('s * 'm Command.t) Base.Decode.t)
        (view:   's -> 'm Vdom.t * string)
        (sub:   's -> 'm Subscription.t)
        (update: ('s, 'm) update)
        (on_url_request: Navigation.url_request -> 'm)
        (on_url_change: Url.t -> 'm)
    : unit
    =
    let init () =
        let url =
            Fmlib_js.Url.current ()
            |> Url.of_string
            |> Option.get (* assume the browser handed us a valid URL *)
        in
        init url on_url_change
    in
    let sub s =
        Subscription.batch
            [
                Subscription.on_url_request on_url_request;
                Subscription.on_url_change on_url_change;
                sub s
            ]
    in
    let open Application in
    main_js
        (make_def
            name
            false
            true
            true
            init
            (make_view2 view)
            update
            sub
        )






let basic_application
        (state:   's)
        (command: 'm Command.t)
        (view:    's -> 'm Vdom.t * string)
        (sub:     's -> 'm Subscription.t)
        (update:  ('s, 'm) update)
    : unit
    =
    let open Application in
    main_onload
        (make_def
            "????"
            false
            false
            false
            (make_init1 state command)
            (make_view2 view)
            update
            sub)
