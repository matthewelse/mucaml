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
  ~(annotations : Annotation.t Nonempty_list.t)
  ~file_id
  ~normalize_ty
  t
  =
  let identified_vars = Identifier.Hash_set.create () in
  Nonempty_list.to_list annotations
  |> List.fold ~init:t ~f:(fun t (annotation : Annotation.t) ->
    match annotation with
    | Expected_type (expr, ty, ~but, ~defined) ->
      let ty = normalize_ty ty in
      let label =
        match but with
        | None -> [%string "this has type %{ty#Type}."]
        | Some but_ty ->
          let but_ty = normalize_ty but_ty in
          [%string
            "this is expected to have type %{ty#Type}, but has type %{but_ty#Type}."]
      in
      let mode = if List.is_empty t.labels then `Primary else `Secondary in
      let t = with_label t ~mode ~file_id ~loc:expr.loc ~label in
      let t =
        (* If something is a variable, add an annotation showing where it was defined. *)
        match expr.desc, defined with
        | Var v, Some loc ->
          (* Avoid adding duplicate annotations for the same variable. *)
          if Hash_set.mem identified_vars v || Location.equal loc Location.initial
          then t
          else (
            Hash_set.add identified_vars v;
            with_label
              t
              ~mode:`Secondary
              ~file_id
              ~loc
              ~label:[%string "%{v#Identifier} was defined here"])
        | _ -> t
      in
      (match but, ty with
       | Some (Base I32), Base I64 ->
         with_note
           t
           ~note:
             [%string "Plain integers have type `i32`. `i64` literals look like `42L`."]
       | Some (Base I64), Base I32 ->
         with_note
           t
           ~note:
             [%string
               "Integers with an L suffix have type `i64`. `i32` literals look like `42`."]
       | _ -> t))
;;
