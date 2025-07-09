open! Ox
open! Import

let debug = false

module T = struct
  type t = { vars : Type.t Union_find.t Type.Var.Table.t }

  let lookup_var t v =
    Hashtbl.find_or_add t.vars v ~default:(fun () -> Union_find.create (Type.Var v))
  ;;

  let unify_var_var t v1 v2 =
    let v1' = lookup_var t v1 in
    let v2' = lookup_var t v2 in
    assert (Type.is_var (Union_find.get v1'));
    assert (Type.is_var (Union_find.get v2'));
    Union_find.union v1' v2'
  ;;

  let unify_var_ty t v ty =
    let repr = lookup_var t v in
    if debug then print_s [%message (v : Type.Var.t) ~equals:(ty : Type.t)];
    Union_find.set repr ty
  ;;

  let var t var = lookup_var t var |> Union_find.get
end

include T
module Unify = Unification.Make (T)

let normalize_ty = Unify.normalize_ty
let create () = { vars = Type.Var.Table.create () }

let rec iter_result xs ~f =
  match xs with
  | [] -> Ok ()
  | x :: xs ->
    let%bind.Result () = f x in
    iter_result xs ~f
;;

let sexp_of_t t =
  let vars =
    Hashtbl.to_alist t.vars
    |> List.map ~f:(fun (var, ty) ->
      let ty = Union_find.get ty in
      var, ty)
  in
  [%sexp { vars : (Type.Var.t * Type.t) list }]
;;

let solve t (constraints : Constraint.t list) ~(env : Env.t) ~file_id =
  let open Result.Let_syntax in
  let%bind () =
    iter_result constraints ~f:(function Same_type (t1, t2, annotations) ->
      Unify.unify_ty_ty t t1 t2 ~env ~annotations ~file_id)
  in
  let values =
    Map.map env.values ~f:(fun typ ->
      let ty = normalize_ty t typ.txt.body ~env in
      if debug
      then
        print_s
          [%message "final normalization" (typ : Type.Poly.t Located.t) (ty : Type.t)];
      { typ with txt = { typ.txt with body = ty } })
  in
  Ok { env with values }
;;
