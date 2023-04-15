open Fmlib_js.Base

module Dictionary = Dictionary.Make (String)

type 'msg handlers = 'msg Handler.Virtual.t list Dictionary.t

module Attributes =
struct
    type 'msg t = {
        styles:   string Dictionary.t;
        props:    Value.t Dictionary.t;
        attrs:    string  Dictionary.t;
        handlers: 'msg handlers
    }

    let of_list (lst: 'msg Attribute.t list): 'msg t =
        let open Attribute in
        List.fold_left
            (fun attrs -> function
                 | Style (name, value) ->
                     {
                         attrs with
                         styles = Dictionary.add name value attrs.styles
                     }
                 | Property (name, value) ->
                     {
                         attrs with
                         props = Dictionary.add name value attrs.props
                     }
                 | Attribute (name, value) ->
                     {
                         attrs with
                         attrs = Dictionary.add name value attrs.attrs
                     }
                 | Handler (name, handler) ->
                     {
                         attrs with
                         handlers =
                             Dictionary.set
                                 name
                                 (function
                                     | None -> [handler]
                                     | Some lst -> handler :: lst)
                                 attrs.handlers
                     }
            )
            {
                styles   = Dictionary.empty;
                props    = Dictionary.empty;
                attrs    = Dictionary.empty;
                handlers = Dictionary.empty;
            }
            lst
end










type ('msg, 'el) t0 =
    (* Pure virtual dom element where all children consist of pairs of a virtual
       dom and real dom node.
     *)
    | Text of string
    | Node of string  * 'msg Attributes.t * ('msg, 'el) t1 list
    | Keyed of string * 'msg Attributes.t * ('msg, 'el) t1 Dictionary.t


and ('msg, 'el) t1 =
    (* Pair of a virtual dom and a corresponding real dom *)
    ('msg, 'el) t0 * 'el



type 'msg t =
    (* Pure virtual dom without any real dom attached to it. *)
    ('msg, unit) t1




(* Create pure virtual dom elements *)

let text (s: string): 'msg t =
    Text s, ()


let node
        (tag: string)
        (attrs: 'msg Attribute.t list)
        (lst: 'msg t list)
    : 'msg t
    =
    Node (tag, Attributes.of_list attrs, lst), ()



let keyed
        (tag: string)
        (attrs: 'msg Attribute.t list)
        (lst: (string * 'msg t) list)
    : 'msg t
    =
    Keyed (tag, Attributes.of_list attrs, Dictionary.of_list lst), ()




(*
    Make and update real dom from a virtual dom
    -------------------------------------------
*)


let element: ('msg, 'el) t1 -> 'el =
    snd


type ('msg, 'el) operations = {
    make_text:     string -> 'el;
    make_element:  string -> 'el list -> 'el;

    add_child:     'el -> 'el -> unit;
    remove_child:  'el -> 'el -> unit;
    replace_child: 'el -> 'el -> 'el -> unit;
    remove_children: 'el -> unit;

    set_style:     'el -> string -> string -> unit;
    set_attribute: 'el -> string -> string -> unit;
    set_property:  'el -> string -> Value.t -> unit;

    remove_style:     'el -> string -> unit;
    remove_attribute: 'el -> string -> unit;
    remove_property:  'el -> string -> unit;

    set_handlers:  'el -> 'msg handlers -> unit;
    update_handlers:  'el -> 'msg handlers -> 'msg handlers -> unit;
}


let add_attributes
        (ops: ('msg, 'el) operations)
        (attrs: 'msg Attributes.t)
        (el: 'el)
    : unit
    =
    Dictionary.(
        iter (ops.set_style el)     attrs.styles;
        iter (ops.set_attribute el) attrs.attrs;
        iter (ops.set_property el)  attrs.props);
    ops.set_handlers el attrs.handlers




let make
        (ops: ('msg, 'el) operations)
        (vdom: 'msg t)
    : ('msg, 'el) t1
    =
    let rec make vdom =
        match vdom with
        | Text s, () ->
            Text s, ops.make_text s

        | Node (tag, attrs, lst), () ->
            let combined_children, real_children =
                make_children lst
            in
            let parent = ops.make_element tag real_children
            in
            add_attributes ops attrs parent;
            Node (tag, attrs, combined_children), parent

        | Keyed (tag, attrs, _), () ->
            let combined_children, real_children =
                assert false
            in
            let parent = ops.make_element tag real_children
            in
            add_attributes ops attrs parent;
            Keyed (tag, attrs, combined_children), parent


    and make_children (lst: 'msg t list): ('msg, 'el) t1 list * 'el list =
        match lst with
        | [] ->
            [], []
        | hd :: tl ->
            let _, hd2 as hd = make hd
            and tl1, tl2 = make_children tl in
            hd :: tl1, hd2 :: tl2
    in
    make vdom




let update_attributes
        (ops:    ('msg, 'el) operations)
        (par:    'el)
        (attrs1: 'msg Attributes.t)
        (attrs2: 'msg Attributes.t)
    : unit
    =
    let open Dictionary in
    let set = ops.set_style par in
    diff
        set set (ops.remove_style par)
        attrs1.styles attrs2.styles;

    let set = ops.set_attribute par in
    diff
        set set (ops.remove_attribute par)
        attrs1.attrs attrs2.attrs;

    let set = ops.set_property par in
    diff
        set set (ops.remove_property par)
        attrs1.props attrs2.props;

    ops.update_handlers par attrs1.handlers attrs2.handlers



let rec update
        (ops: ('msg, 'el) operations)
        (vdom: 'msg t)
        (dom: ('msg, 'el) t1)
    : ('msg, 'el) t1 * bool
    =
    match vdom, dom with
    | (Text s1, ()), (Text s2, _) when s1 = s2 ->

        dom, false

    | (Node (tag1, attrs1, lst1), ()),
      (Node (tag2, attrs2, lst2), par) when tag1 = tag2 ->

        let children = List.rev (update_children ops par lst1 lst2 [])
        in
        update_attributes ops par attrs1 attrs2;
        (Node (tag2, attrs1, children), par), false

    | (Keyed (tag1, attrs1, d1), ()),
      (Keyed (tag2, attrs2, d2), par) when tag1 = tag2 ->

        let children = update_keyed ops par d1 d2
        in
        update_attributes ops par attrs1 attrs2;
        (Keyed (tag2, attrs1, children), par), false

    | _, _ ->

        make ops vdom, true


and update_keyed
        (ops: ('msg, 'el) operations)
        (par: 'el)
        (d1: 'msg t Dictionary.t)
        (d2: ('msg, 'el) t1 Dictionary.t)
    : ('msg, 'el) t1 Dictionary.t
    =
    ops.remove_children par;
    let d = ref Dictionary.empty
    in
    Dictionary.iter
        (fun key vdom ->
             match Dictionary.find_opt key d2 with
             | None ->

                 let (_, el) as dom = make ops vdom in
                 ops.add_child el par;
                 d := Dictionary.add key dom !d;

             | Some dom ->

                 let ((_, el) as dom), _ = update ops vdom dom in
                 ops.add_child el par;
                 d := Dictionary.add key dom !d;
        )
        d1;
    !d



and update_children
        (ops: ('msg, 'el) operations)
        (par: 'el)
        (lst1: 'msg t list)
        (lst2: ('msg, 'el) t1 list)
        (nlst2: ('msg, 'el) t1 list)
    : ('msg, 'el) t1 list
    =
    match lst1, lst2 with
    | [], [] ->
        nlst2

    | [], (_, hd) :: tl ->
        (* Old dom has too many children *)
        ops.remove_child hd par;
        update_children ops par [] tl nlst2

    | hd1 :: tl1, [] ->
        (* New vdom has more children than the old dom *)
        let (_, hd11) as hd1 = make ops hd1 in
        ops.add_child hd11 par;
        update_children ops par tl1 [] (hd1 :: nlst2)

    | hd1 :: tl1,   ((_, old_el) as hd2) :: tl2 ->
        let (_, new_el) as hd2 , created = update ops hd1 hd2 in
        if created then
            ops.replace_child old_el new_el par;
        update_children ops par tl1 tl2 (hd2 :: nlst2)
