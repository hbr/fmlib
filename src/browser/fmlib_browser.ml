module Random =
struct
    include Random
end






module Time =
struct
    include Time
end







module Task =
struct
    include Task
end





module Value =
struct
    include Fmlib_js.Base.Value

    let record = _object
end




module Event_flag =
struct
    include Event_flag
end






module Decoder =
struct
    include Fmlib_js.Base.Decode


    let run (decode: 'a t) (v: Value.t): 'a option =
        decode v
end






module Command =
struct
    include Command
end






module Subscription =
struct
    include Subscription
end





module Attribute =
struct
    include Attribute

    let on (key: string) (decode: 'msg Decoder.t): 'msg t =
        handler
            key
            Event_flag.no_stop
            Event_flag.no_prevent
            decode

    let on_click (msg: 'msg): 'msg t =
        on "click" (Decoder.return msg)


    (* Styles *)

    let font_size (size: string): 'm t =
        style "font-size" size

    let color (color: string): 'm t =
        style "color" color

    let background_color (color: string): 'm t =
        style "background-color" color

    let height (value: string): 'm t =
        style "height" value

    let width (value: string): 'm t =
        style "width" value

    let margin (value: string): 'm t =
        style "margin" value

    let padding (value: string): 'm t =
        style "padding" value

    let border_style (value: string): 'm t =
        style "border-style" value

    let border_width (value: string): 'm t =
        style "border-width" value

    let border_color (value: string): 'm t =
        style "border-color" value


    (* Attributes *)

    let id (value: string): 'm t =
        attribute "id" value

    let class_ (value: string): 'm t =
        attribute "class" value

    let href (value: string): 'm t =
        attribute "href" value

    let src (value: string): 'm t =
        attribute "src" value

    let title (value: string): 'm t =
        attribute "title" value

    let value (value: string): 'm t =
        attribute "value" value

    let placeholder (value: string): 'm t =
        attribute "placeholder" value


    let on_input (f: string -> 'msg): 'msg t =
        let decode =
            let open Decoder in
            field "target" (field "value" (map f string))
        in
        handler
            "input"
            Event_flag.stop
            Event_flag.no_prevent
            decode
end







module Html =
struct
    include Vdom


    let h1 attrs nodes = node "h1" attrs nodes
    let h2 attrs nodes = node "h2" attrs nodes
    let h3 attrs nodes = node "h3" attrs nodes
    let h4 attrs nodes = node "h4" attrs nodes
    let h5 attrs nodes = node "h5" attrs nodes
    let h6 attrs nodes = node "h6" attrs nodes

    let div attrs nodes  = node "div" attrs nodes
    let span attrs nodes = node "span" attrs nodes
    let pre attrs nodes  = node "pre" attrs nodes
    let p attrs nodes    = node "p" attrs nodes

    let button attrs nodes   = node "button" attrs nodes
    let input attrs nodes    = node "input" attrs nodes
    let label attrs nodes    = node "label" attrs nodes
    let textarea attrs nodes = node "textarea" attrs nodes
    let select attrs nodes   = node "select" attrs nodes

    let ol attrs nodes  = node "ol" attrs nodes
    let ul attrs nodes  = node "ul" attrs nodes
    let li attrs nodes  = node "li" attrs nodes
end


include Browser
