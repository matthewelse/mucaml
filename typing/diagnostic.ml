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

let with_annotations
  ?mode
  ~(annotations : Annotation.t Nonempty_list.t)
  ~file_id
  ~normalize_ty
  t
  =
  Nonempty_list.to_list annotations
  |> List.fold ~init:t ~f:(fun t (annotation : Annotation.t) ->
    match annotation with
    | Expected_type (expr, ty, ~but) ->
      let ty = normalize_ty ty in
      let label =
        match but with
        | None -> [%string "has type %{ty#Type}."]
        | Some but_ty ->
          let but_ty = normalize_ty but_ty in
          [%string "expected to have type %{ty#Type}, but has type %{but_ty#Type}."]
      in
      let t = with_label t ?mode ~file_id ~loc:expr.loc ~label in
      (match expr.desc, ty with
       | Literal (Int32 n), Base I64 ->
         with_note
           t
           ~note:[%string "Plain integers are i32s. i64 literals look like %{n#I32}L."]
       | Literal (Int64 n), Base I32 ->
         with_note
           t
           ~note:
             [%string
               "Integers with an L suffix are i64s. i32 literals look like %{n#I64}."]
       | _ -> t))
;;
