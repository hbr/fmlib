open Fmlib_js.Base

type ('msg, 'el) t1

type 'msg t = ('msg, unit) t1


type 'msg handlers = 'msg Handler.Virtual.t list Dictionary.t


type ('msg, 'el) operations = {
    make_text:     string -> 'el;
    make_element:  string -> 'el list -> 'el;
    make_element_ns:  string -> string -> 'el list -> 'el;

    add_child:     'el -> 'el -> unit;
    remove_child:  'el -> 'el -> unit;
    replace_child: 'el -> 'el -> 'el -> unit;
    remove_children: 'el -> unit;

    set_text: 'el -> string -> unit;

    set_style:     'el -> string -> string -> unit;
    set_attribute: 'el -> string -> string -> unit;
    set_property:  'el -> string -> Value.t -> unit;

    remove_style:     'el -> string -> unit;
    remove_attribute: 'el -> string -> unit;
    remove_property:  'el -> string -> unit;

    set_handlers:  'el -> 'msg handlers -> unit;
    update_handlers:  'el -> 'msg handlers -> 'msg handlers -> unit;
}




val element: ('msg, 'el) t1 -> 'el


val make: ('msg, 'el) operations -> 'msg t -> ('msg, 'el) t1


val update:
    ('msg, 'el) operations
    -> 'msg t
    -> ('msg, 'el) t1
    -> ('msg, 'el) t1 * bool (* boolean flag if element has been newly created.
                                An element and all its kids have to be newly
                                created, if the tags of the vdom and the actual
                                dom are different. *)




val text: string -> 'msg t

val node: string -> 'msg Attribute.t list -> 'msg t list -> 'msg t

val node_ns: string -> string -> 'msg Attribute.t list -> 'msg t list -> 'msg t

val keyed: string -> 'msg Attribute.t list -> (string * 'msg t) list -> 'msg t

val map: ('a -> 'b) -> ('a, unit) t1 -> ('b, unit) t1
