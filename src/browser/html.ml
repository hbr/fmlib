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

let svg_node tag attrs nodes =
    node_ns "http://www.w3.org/2000/svg" tag attrs nodes
