include Vdom

let address attrs nodes = node "address" attrs nodes
let article attrs nodes = node "article" attrs nodes
let aside attrs nodes   = node "aside" attrs nodes
let footer attrs nodes  = node "footer" attrs nodes
let header attrs nodes  = node "header" attrs nodes
let h1 attrs nodes      = node "h1" attrs nodes
let h2 attrs nodes      = node "h2" attrs nodes
let h3 attrs nodes      = node "h3" attrs nodes
let h4 attrs nodes      = node "h4" attrs nodes
let h5 attrs nodes      = node "h5" attrs nodes
let h6 attrs nodes      = node "h6" attrs nodes
let hgroup attrs nodes  = node "hgroup" attrs nodes
let main attrs nodes    = node "main" attrs nodes
let nav attrs nodes     = node "nav" attrs nodes
let section attrs nodes = node "section" attrs nodes
let search attrs nodes  = node "search" attrs nodes


let blockquote attrs nodes = node "blockquote" attrs nodes
let dd attrs nodes         = node "dd" attrs nodes
let div attrs nodes        = node "div" attrs nodes
let dl attrs nodes         = node "dl" attrs nodes
let dt attrs nodes         = node "dt" attrs nodes
let figcaption attrs nodes = node "figcaption" attrs nodes
let figure attrs nodes     = node "figure" attrs nodes
let hr attrs nodes         = node "hr" attrs nodes
let li attrs nodes         = node "li" attrs nodes
let menu attrs nodes       = node "menu" attrs nodes
let ol attrs nodes         = node "ol" attrs nodes
let p attrs nodes          = node "p" attrs nodes
let pre attrs nodes        = node "pre" attrs nodes
let ul attrs nodes         = node "ul" attrs nodes


let a attrs nodes      = node "a" attrs nodes
let abbr attrs nodes   = node "abbr" attrs nodes
let b attrs nodes      = node "b" attrs nodes
let bdi attrs nodes    = node "bdi" attrs nodes
let bdo attrs nodes    = node "bdo" attrs nodes
let br attrs nodes     = node "br" attrs nodes
let cite attrs nodes   = node "cite" attrs nodes
let code attrs nodes   = node "code" attrs nodes
let data attrs nodes   = node "data" attrs nodes
let dfn attrs nodes    = node "dfn" attrs nodes
let em attrs nodes     = node "em" attrs nodes
let i attrs nodes      = node "i" attrs nodes
let kbd attrs nodes    = node "kbd" attrs nodes
let mark attrs nodes   = node "mark" attrs nodes
let q attrs nodes      = node "q" attrs nodes
let rp attrs nodes     = node "rp" attrs nodes
let rt attrs nodes     = node "rt" attrs nodes
let ruby attrs nodes   = node "ruby" attrs nodes
let s attrs nodes      = node "s" attrs nodes
let samp attrs nodes   = node "samp" attrs nodes
let small attrs nodes  = node "small" attrs nodes
let span attrs nodes   = node "span" attrs nodes
let strong attrs nodes = node "strong" attrs nodes
let sub attrs nodes    = node "sub" attrs nodes
let sup attrs nodes    = node "sup" attrs nodes
let time attrs nodes   = node "time" attrs nodes
let u attrs nodes      = node "u" attrs nodes
let var attrs nodes    = node "var" attrs nodes
let wbr attrs nodes    = node "wbr" attrs nodes


let area attrs nodes  = node "area" attrs nodes
let audio attrs nodes = node "audio" attrs nodes
let img attrs nodes   = node "img" attrs nodes
let map_ attrs nodes  = node "map" attrs nodes
let track attrs nodes = node "track" attrs nodes
let video attrs nodes = node "video" attrs nodes


let embed attrs nodes       = node "embed" attrs nodes
let fencedframe attrs nodes = node "fencedframe" attrs nodes
let iframe attrs nodes      = node "iframe" attrs nodes
let object_ attrs nodes     = node "object" attrs nodes
let picture attrs nodes     = node "picture" attrs nodes
let source attrs nodes      = node "source" attrs nodes


let canvas attrs nodes = node "canvas" attrs nodes


let del attrs nodes = node "del" attrs nodes
let ins attrs nodes = node "ins" attrs nodes


let caption attrs nodes = node "caption" attrs nodes
let col attrs nodes = node "col" attrs nodes
let colgroup attrs nodes = node "colgroup" attrs nodes
let table attrs nodes = node "table" attrs nodes
let tbody attrs nodes = node "tbody" attrs nodes
let td attrs nodes = node "td" attrs nodes
let tfoot attrs nodes = node "tfoot" attrs nodes
let th attrs nodes = node "th" attrs nodes
let thead attrs nodes = node "thead" attrs nodes
let tr attrs nodes = node "tr" attrs nodes


let button attrs nodes          = node "button" attrs nodes
let datalist attrs nodes        = node "datalist" attrs nodes
let fieldset attrs nodes        = node "fieldset" attrs nodes
let form attrs nodes            = node "form" attrs nodes
let input attrs nodes           = node "input" attrs nodes
let label attrs nodes           = node "label" attrs nodes
let legend attrs nodes          = node "legend" attrs nodes
let meter attrs nodes           = node "meter" attrs nodes
let optgroup attrs nodes        = node "optgroup" attrs nodes
let option attrs nodes          = node "option" attrs nodes
let output attrs nodes          = node "output" attrs nodes
let progress attrs nodes        = node "progress" attrs nodes
let select attrs nodes          = node "select" attrs nodes
let selectedcontent attrs nodes = node "selectedcontent" attrs nodes
let textarea attrs nodes        = node "textarea" attrs nodes


let details attrs nodes = node "details" attrs nodes
let dialog attrs nodes = node "dialog" attrs nodes
let summary attrs nodes = node "summary" attrs nodes


let svg_node tag attrs nodes =
    node_ns "http://www.w3.org/2000/svg" tag attrs nodes
