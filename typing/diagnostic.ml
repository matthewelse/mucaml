open! Ox
open! Import
open Grace.Diagnostic

(* TODO: move this to some upstream library to share with frontend *)

let error message : t =
  { severity = Error
  ; message = (fun fmt -> Format.pp_print_string fmt message)
  ; labels = []
  ; notes = []
  }
;;

let with_label ?(mode = `Primary) ~file_id ~loc ~label t =
  let label =
    match mode with
    | `Primary ->
      Label.primary ~id:file_id ~range:loc (fun fmt -> Format.pp_print_string fmt label)
    | `Secondary ->
      Label.secondary ~id:file_id ~range:loc (fun fmt -> Format.pp_print_string fmt label)
  in
  { t with labels = label :: t.labels }
;;

let with_note ~note t =
  let note fmt = Format.pp_print_string fmt note in
  { t with notes = note :: t.notes }
;;
