(** Javascript wrappers. *)


(** {1 Common Modules}

    Usable in a browser window, a worker thread in the browser and node.
*)


module Base = Base

module Timer = Timer

module Date  = Date


(** {1 Common Browser Modules}

    Usable in a browser window or in a worker thread in the browser.
*)

module Event = Event

module Event_target = Event_target

module Http_request = Http_request

module Web_worker = Web_worker



(** {1 Browser Window Modules} *)

module Dom = Dom
