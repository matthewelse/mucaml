open! Ox
open! Import

module Annotation = struct
  type t = Expression_should_have_type of Ast.Expr.t * Type.t [@@deriving sexp_of]
end

module Constraint = struct
  type t = Same_type of Type.t * Type.t [@@deriving sexp_of]
end

module Env = struct
  module Mutable = struct
    type t = { mutable next_tv : Type.Var.t } [@@deriving sexp_of]

    let empty () = { next_tv = Type.Var.zero }

    let fresh_tv t =
      let tv = t.next_tv in
      t.next_tv <- Type.Var.succ t.next_tv;
      tv
    ;;
  end

  type t =
    { values : Type.Poly.t Identifier.Map.t @@ global
    ; mut : Mutable.t @@ global
    }
  [@@deriving globalize, sexp_of]

  let empty () = { values = Identifier.Map.empty; mut = Mutable.empty () }
  let fresh_tv t = Mutable.fresh_tv t.mut
  let with_var t ~var ~ty = { t with values = Map.set t.values ~key:var ~data:ty }
  let var t var = Map.find t.values var
end

module Diagnostic = struct
  open Grace.Diagnostic

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
        Label.secondary ~id:file_id ~range:loc (fun fmt ->
          Format.pp_print_string fmt label)
    in
    { t with labels = label :: t.labels }
  ;;

  let with_note ~note t =
    let note fmt = Format.pp_print_string fmt note in
    { t with notes = note :: t.notes }
  ;;
end

let rec infer' (expr : Ast.Expr.t) ~env ~(constraints : Constraint.t Queue.t) ~file_id
  : (Typed_ast.Expr.t * Type.t, Grace.Diagnostic.t) result
  =
  let open Result.Let_syntax in
  let open Diagnostic in
  match expr.desc with
  | Literal (Int32 n) -> Ok ({ desc = Literal (Int32 n); loc = expr.loc }, Base I32)
  | Literal (Int64 n) -> Ok ({ desc = Literal (Int64 n); loc = expr.loc }, Base I64)
  | Literal (Bool b) -> Ok ({ desc = Literal (Bool b); loc = expr.loc }, Base Bool)
  | Literal Unit -> Ok ({ desc = Literal Unit; loc = expr.loc }, Base Unit)
  | Fun { params; body } ->
    let env, params =
      List.fold_map params ~init:env ~f:(fun env (name, ty) ->
        let (ty : Type.t) =
          match ty with
          | None -> Var (Env.fresh_tv env)
          | Some ty ->
            (* Skip generating a type variable (and a constraint), and just use the
               provided type. *)
            Type.of_ast ty
        in
        Env.with_var env ~var:name.txt ~ty:(Type.Poly.mono ty), (name, ty))
    in
    let%bind body_typed_ast, body_type = infer' body ~env ~constraints ~file_id in
    Ok
      ( { Typed_ast.Expr.desc = Fun { params; body = body_typed_ast; body_type }
        ; loc = expr.loc
        }
      , Type.Fun (List.map params ~f:snd, body_type) )
  | Var v ->
    (match Env.var env v with
     | None ->
       Error
         (error [%string "Unknown variable %{v#Identifier}"]
          |> with_label ~file_id ~loc:expr.loc ~label:"used here")
     | Some ty ->
       let ty = Type.Poly.init ty ~fresh_var:(fun () -> Env.fresh_tv env) in
       Ok ({ desc = Var v; loc = expr.loc }, ty))
  | _ ->
    Error
      (error [%string "Unsupported expression"]
       |> with_label ~file_id ~loc:expr.loc ~label:"specified here")
;;

let infer (expr : Ast.Expr.t) ~env ~file_id =
  let open Result.Let_syntax in
  let constraints = Queue.create () in
  let%bind typed_expr, ty = infer' expr ~env ~constraints ~file_id in
  Ok (typed_expr, ty, Queue.to_list constraints)
;;

let type_ast (ast : Ast.t) ~file_id =
  let open Result.Let_syntax in
  let open Diagnostic in
  List.fold_result ast ~init:(Env.empty ()) ~f:(fun env (top_level : Ast.Toplevel.t) ->
    match top_level with
    | External { loc; _ } ->
      Error
        (error [%string "Unsupported external"]
         |> with_label ~file_id ~loc ~label:"specified here"
         |> with_note ~note:"The FFI isn't supported by the type checker yet.")
    | Function { name; params; body; loc } ->
      let env =
        let tv = Env.fresh_tv env in
        Env.with_var env ~var:name.txt ~ty:(Type.Poly.mono (Var tv))
      in
      let%bind _typed_expr, ty, constraints =
        infer { desc = Fun { params; body }; loc } ~env ~file_id
      in
      print_s [%message (constraints : Constraint.t list)];
      print_endline (Type.to_string ty);
      Ok env)
;;
