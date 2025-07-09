open! Ox
open! Import

let debug = false

module Make (Lookup : sig
    type t

    val unify_var_var : t -> Type.Var.t -> Type.Var.t -> unit
    val unify_var_ty : t -> Type.Var.t -> Type.t -> unit
    val var : t -> Type.Var.t -> Type.t
  end) =
struct
  let rec iter_result xs ~f =
    match xs with
    | [] -> Ok ()
    | x :: xs ->
      let%bind.Result () = f x in
      iter_result xs ~f
  ;;

  let rec iter2_result xs ys ~f =
    match xs, ys with
    | [], [] -> Ok ()
    | x :: xs, y :: ys ->
      let%bind.Result () = f x y |> Result.map_error ~f:(fun err -> `Error err) in
      iter2_result xs ys ~f
    | [], _ :: _ | _ :: _, [] -> Error `Mismatched_lengths
  ;;

  let rec normalize_ty t (ty : Type.t) ~env : Type.t =
    match ty with
    | Base base -> Base base
    | Var v ->
      (match Lookup.var t v with
       | Var _ as ty -> ty
       | ty ->
         if debug then print_s [%message "normalising" (v : Type.Var.t) (ty : Type.t)];
         normalize_ty t ty ~env)
    | Fun (args, result) ->
      let args = List.map ~f:(normalize_ty t ~env) args in
      let result = normalize_ty t result ~env in
      Fun (args, result)
  ;;

  let rec unify_ty_ty t ty1 ty2 ~env ~annotations ~file_id =
    let open Result.Let_syntax in
    let open Diagnostic in
    if debug then print_s [%message "unify_ty_ty" (ty1 : Type.t) (ty2 : Type.t)];
    let ty1 = normalize_ty t ty1 ~env in
    let ty2 = normalize_ty t ty2 ~env in
    (* FIXME: which one is the one we care about? *)
    match ty1, ty2 with
    | Fun (args_left, result_left), Fun (args_right, result_right) ->
      let%bind () =
        iter2_result args_left args_right ~f:(fun arg_left arg_right ->
          (* FIXME melse: Add an extra annotation to highlight progress *)
          unify_ty_ty t arg_left arg_right ~env ~annotations ~file_id)
        |> Result.map_error ~f:(function
          | `Error err -> err
          | `Mismatched_lengths ->
            error "Incorrect number of arguments passed to a function."
            |> with_annotations ~normalize_ty:(normalize_ty t ~env) ~annotations ~file_id)
      in
      unify_ty_ty t result_left result_right ~env ~annotations ~file_id
    | Base l, Base r ->
      if Type.Base.equal l r
      then Ok ()
      else
        Error
          (error "Type error"
           |> with_annotations ~normalize_ty:(normalize_ty t ~env) ~annotations ~file_id)
    | Var v1, Var v2 ->
      Lookup.unify_var_var t v1 v2;
      Ok ()
    | Var v, ty | ty, Var v ->
      if Type.occurs ty ~var:v
      then
        Error
          (error "Type error"
           |> with_note ~note:"we <3 recursion, but not _that_ much."
           |> with_note
                ~note:
                  "This would create an infinitely nested type like `'a list list list \
                   list...`"
           |> with_annotations ~normalize_ty:(normalize_ty t ~env) ~annotations ~file_id)
      else (
        Lookup.unify_var_ty t v ty;
        Ok ())
    | _, _ -> Error (error "Type error" |> with_note ~note:"Type mismatch")
  ;;
end
