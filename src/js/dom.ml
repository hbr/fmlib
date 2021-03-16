open Js_of_ocaml
open Base

type js_string = Js.js_string Js.t



class type style =
object
    method setProperty:    js_string -> js_string -> unit Js.meth
    method removeProperty: js_string -> unit Js.meth
end


class type event =
object
    method stopPropagation: unit -> unit Js.meth
    method preventDefault:  unit -> unit Js.meth
end


class type event_target =
object
    method addEventListener:    js_string -> (event Js.t -> unit) -> unit Js.meth
    method removeEventListener: js_string -> (event Js.t -> unit) -> unit Js.meth
end




class type node =
object
    inherit event_target

    method parentNode:  node Js.t Js.Opt.t Js.readonly_prop
    method firstChild:  node Js.t Js.Opt.t Js.readonly_prop
    method nextSibling: node Js.t Js.Opt.t Js.readonly_prop

    method appendChild: node Js.t -> unit Js.meth
    method removeChild: node Js.t -> unit Js.meth
end





class type text_node =
object
    inherit node
end




class type element =
object
    inherit node

    method scrollWidth:     float Js.readonly_prop
    method clientWidth:     float Js.readonly_prop
    method scrollLeft:      float Js.readonly_prop
    method scrollTop:       float Js.readonly_prop

    method style:           style Js.t Js.readonly_prop
    method setAttribute:    js_string -> js_string -> unit Js.meth
    method removeAttribute: js_string -> unit Js.meth

    method focus: unit -> unit Js.meth
    method blur:  unit -> unit Js.meth
end






class type document =
object
    inherit event_target
    method body:           element Js.t Js.readonly_prop
    method createTextNode: js_string -> text_node Js.t Js.meth
    method createElement:  js_string -> element Js.t Js.meth
end




class type window =
object
    inherit event_target
    method document: document Js.t Js.readonly_prop
    method requestAnimationFrame: (float -> unit) -> unit Js.meth
end






module Style =
struct
    type t = style Js.t

    let set (name: string) (value: string) (s: t): unit =
        s##setProperty (Js.string name) (Js.string value)

    let remove (name: string) (s: t): unit =
        s##removeProperty (Js.string name)
end




module Event =
struct
    type t = event Js.t

    let value (e: t): Value.t =
        Obj.magic e

    let stop_propagation (e: t): unit =
        e##stopPropagation ()

    let prevent_default (e: t): unit =
        e##preventDefault ()
end





module Event_target =
struct
    type t = event_target Js.t

    let add (name: string) (handler: Event.t -> unit) (tgt: t): unit =
        tgt##addEventListener (Js.string name) handler

    let remove (name: string) (handler: Event.t -> unit) (tgt: t): unit =
        tgt##removeEventListener (Js.string name) handler
end





module Node =
struct
    type t = node Js.t

    let event_target (node: t): Event_target.t =
        Js.Unsafe.coerce node

    let parent (node: t): t option =
        let open Js in
        Opt.to_option node##.parentNode

    let first (node: t): t option =
        let open Js in
        Opt.to_option node##.firstChild

    let next (node: t): t option =
        let open Js in
        Opt.to_option node##.nextSibling

    let append (child: t) (node: t): unit =
        node##appendChild child

    let remove (child: t) (node: t): unit =
        node##removeChild child
end







module Element =
struct
    type t = element Js.t

    let node (element: t): Node.t =
        Js.Unsafe.coerce element

    let scroll_width (element: t): float =
        element##.scrollWidth

    let client_width (element: t): float =
        element##.clientWidth

    let scroll_left (element: t): float =
        element##.scrollLeft

    let scroll_top (element: t): float =
        element##.scrollTop

    let style (element: t): Style.t =
        element##.style

    let set_attribute (name: string) (value: string) (element: t): unit =
        element##setAttribute (Js.string name) (Js.string value)

    let remove_attribute (name: string) (element: t): unit =
        element##removeAttribute (Js.string name)


    let set_property (name: string) (value: Value.t) (element: t): unit =
        Js.Unsafe.set element (Js.string name) value

    let delete_property (name: string) (element: t): unit =
        Js.Unsafe.delete element (Js.string name)


    let focus (element: t): unit =
        element##focus ()

    let blur (element: t): unit =
        element##blur ()
end





module Text_node =
struct
    type t = text_node Js.t
    let node (tn: t): Node.t =
        Js.Unsafe.coerce tn
end





module Document =
struct
    type t = document Js.t


    let body (doc: t): Element.t =
        doc##.body

    let create_element (tag: string) (doc: t): Element.t =
        doc##createElement (Js.string tag)

    let create_text_node (text: string) (doc: t): Text_node.t =
        doc##createTextNode (Js.string text)
end








module Window =
struct
    type t = window Js.t


    let get (): t =
        Js.Unsafe.global

    let event_target (w: t): Event_target.t =
        Js.Unsafe.coerce w


    let add_listener (name: string) (handler: Event.t -> unit) (w: t): unit =
        Event_target.add name handler (event_target w)

    let remove_listener (name: string) (handler: Event.t -> unit) (w: t): unit =
        Event_target.remove name handler (event_target w)

    let document (w: t): Document.t =
        w##.document

    let on_next_animation (callback: float -> unit) (w: t): unit =
        w##requestAnimationFrame callback
end
