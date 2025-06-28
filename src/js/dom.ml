open Js_of_ocaml
open Base

type js_string = Js.js_string Js.t



class type style =
object
    method setProperty:    js_string -> js_string -> unit Js.meth
    method removeProperty: js_string -> unit Js.meth
end




class type node =
object
    method parentNode:  node Js.t Js.Opt.t Js.readonly_prop
    method firstChild:  node Js.t Js.Opt.t Js.readonly_prop
    method nextSibling: node Js.t Js.Opt.t Js.readonly_prop

    method appendChild:  node Js.t -> unit Js.meth
    method removeChild:  node Js.t -> unit Js.meth
    method replaceChild: node Js.t -> node Js.t -> unit Js.meth
    method nodeValue:    js_string Js.prop
end







class type element =
object
    method scrollWidth:     int Js.readonly_prop
    method scrollHeight:    int Js.readonly_prop
    method clientWidth:     int Js.readonly_prop
    method clientHeight:    int Js.readonly_prop
    method scrollLeft:      int Js.prop
    method scrollTop:       int Js.prop

    method style:           style Js.t Js.readonly_prop
    method setAttribute:    js_string -> js_string -> unit Js.meth
    method removeAttribute: js_string -> unit Js.meth

    method focus: 'a Js.opt -> unit Js.meth
    method blur:  'a Js.opt -> unit Js.meth
end






class type document =
object
    method title:          js_string Js.prop
    method body:           element Js.t Js.readonly_prop
    method getElementById: js_string -> element Js.t Js.Opt.t Js.meth
    method createTextNode: js_string -> node Js.t Js.meth
    method createElement:  js_string -> element Js.t Js.meth
    method createElementNS:  js_string -> js_string -> element Js.t Js.meth
    method createDocumentFragment:  'a Js.opt -> node Js.t Js.meth
end





class type history =
object
    method go: int -> unit Js.meth
    method pushState:    Base.Value.t -> js_string -> js_string -> unit Js.meth
    method replaceState: Base.Value.t -> js_string -> js_string -> unit Js.meth
end




class type location =
object
    method href:        js_string Js.readonly_prop
    method protocol:    js_string Js.readonly_prop
    method host:        js_string Js.readonly_prop
    method port:        js_string Js.readonly_prop
    method pathname:    js_string Js.readonly_prop
    method search:      js_string Js.readonly_prop
    method hash:        js_string Js.readonly_prop

    method assign: js_string -> unit Js.meth
    method reload: unit -> unit Js.meth
end





class type window =
object
    method history:  history Js.t Js.readonly_prop
    method location: location Js.t Js.readonly_prop
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




module Node =
struct
    type t = node Js.t

    let event_target (node: t): Event_target.t =
        Obj.magic node

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
        assert (not (node == Obj.magic Value.null));
        node##appendChild child

    let remove (child: t) (node: t): unit =
        node##removeChild child

    let replace (new_child: t) (old_child: t) (node: t): unit =
        node##replaceChild new_child old_child

    let rec remove_children (parent: t): unit =
        match first parent with
        | None ->
            ()
        | Some child ->
            remove child parent;
            remove_children parent (* tail recursion, compiled to javascript
                                      loop. *)

    let node_value (node: t): string =
        Js.to_string node##.nodeValue

    let set_node_value (value: string) (node: t): unit =
        node##.nodeValue := (Js.string value)
end







module Element =
struct
    type t = element Js.t

    let node (element: t): Node.t =
        Js.Unsafe.coerce element

    let scroll_width (element: t): int =
        element##.scrollWidth

    let scroll_height (element: t): int =
        element##.scrollHeight

    let client_width (element: t): int =
        element##.clientWidth

    let client_height (element: t): int =
        element##.clientHeight

    let scroll_left (element: t): int =
        element##.scrollLeft

    let scroll_top (element: t): int =
        element##.scrollTop


    let set_scroll_left (v: int) (element: t): unit =
        element##.scrollLeft := v

    let set_scroll_top (v: int) (element: t): unit =
        element##.scrollTop := v




    let style (element: t): Style.t =
        element##.style

    let set_attribute (name: string) (value: string) (element: t): unit =
        element##setAttribute (Js.string name) (Js.string value)

    let remove_attribute (name: string) (element: t): unit =
        element##removeAttribute (Js.string name)


    let property
            (name: string)
            (element: t)
        : Base.Value.t option =
        Js.Unsafe.get element (Js.string name) |> Js.Opt.to_option

    let set_property (name: string) (value: Value.t) (element: t): unit =
        Js.Unsafe.set element (Js.string name) value

    let delete_property (name: string) (element: t): unit =
        Js.Unsafe.delete element (Js.string name)


    let focus (element: t): unit =
        element##focus Js.null

    let blur (element: t): unit =
        element##blur Js.null
end







module Document =
struct
    type t = document Js.t


    let title (doc: t): string =
        Js.to_string doc##.title


    let set_title (title: string) (doc: t): unit =
        doc##.title := Js.string title


    let body (doc: t): Element.t =
        assert (doc##.body != Js.Unsafe.js_expr "null");
        doc##.body

    let find (name: string) (doc: t): Element.t option =
        Js.Opt.to_option (doc##getElementById (Js.string name))

    let create_element (tag: string) (doc: t): Element.t =
        doc##createElement (Js.string tag)

    let create_text_node (text: string) (doc: t): Node.t =
        doc##createTextNode (Js.string text)

    let create_element_ns (namespace: string) (tag: string) (doc: t): Element.t =
        doc##createElementNS (Js.string namespace) (Js.string tag)

    let create_document_fragment (doc: t): Node.t =
        doc##createDocumentFragment Js.null
end



module History =
struct
    type t = history Js.t


    let go (i: int) (history: t): unit =
        history##go i


    let push_state
            (state: Value.t)
            (title: string)
            (url: string)
            (history: t)
        : unit =
        let open Js in
        history##pushState state (string title) (string url)


    let replace_state
            (state: Value.t)
            (title: string)
            (url: string)
            (history: t)
        : unit =
        let open Js in
        history##replaceState state (string title) (string url)
end





module Location =
struct
    type t = location Js.t

    let href (location: t): string =
        Js.to_string location##.href

    let protocol (location: t): string =
        Js.to_string location##.protocol

    let host (location: t): string =
        Js.to_string location##.host

    let port (location: t): string =
        Js.to_string location##.port

    let pathname (location: t): string =
        Js.to_string location##.pathname

    let search (location: t): string =
        Js.to_string location##.search

    let hash (location: t): string =
        Js.to_string location##.hash

    let assign (url: string) (location: t): unit =
        location##assign (Js.string url)


    let reload (location: t): unit =
        location##reload ()
end




module Window =
struct
    type t = window Js.t


    let get (): t =
        Js.Unsafe.global

    let event_target (w: t): Event_target.t =
        Obj.magic w


    let document (w: t): Document.t =
        w##.document


    let history (w: t): History.t =
        w##.history


    let location (w: t): Location.t =
        w##.location


    let on_next_animation (callback: float -> unit) (w: t): unit =
        w##requestAnimationFrame callback
end
