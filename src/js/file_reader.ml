type file = File.t


open Js_of_ocaml


class type fileReader =
object
    method readyState: int Js.readonly_prop

    method readAsText: file -> unit Js.meth

    method error: File.fileError Js.t Js.opt Js.readonly_prop

    method result: Base.Value.t Js.readonly_prop
end


type t = fileReader Js.t


let event_target (reader: t): Event_target.t =
    Obj.magic reader


let make (): t =
    let reader = Js.Unsafe.global##.FileReader in
    new%js reader


let ready_state (reader: t): int =
    reader##.readyState


let read_text (reader: t) (file: file) (): unit =
    reader##readAsText file


let result (reader: t): Base.Value.t option =
    match reader##.error |> Js.Opt.to_option with
    | None ->
        Some (reader##.result)
    | Some _ ->
        None
