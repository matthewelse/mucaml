open! Ox
open! Import

let infer_literal (literal : Ast.Literal.t) ~loc : (Typed_ast.Expr.t * Type.t, _) result =
  let (lit, ty) : Typed_ast.Literal.t * Type.Base.t =
    match literal with
    | Int32 n -> Int32 n, I32
    | Int64 n -> Int64 n, I64
    | Bool b -> Bool b, Bool
    | Unit -> Unit, Unit
  in
  Ok ({ desc = Literal lit; loc }, Base ty)
;;

let infer_var ~var ~env ~file_id ~loc : (Typed_ast.Expr.t * Type.t, _) result =
  let open Diagnostic in
  match Env.var env var with
  | None ->
    Error
      (error [%string "Unknown variable %{var#Identifier}"]
       |> with_label ~file_id ~loc ~label:"used here")
  | Some ty ->
    let ty = Type.Poly.init ty ~fresh_var:(fun () -> Env.fresh_tv env) in
    Ok ({ desc = Var var; loc }, ty)
;;

let rec infer' (expr : Ast.Expr.t) ~env ~(constraints : Constraint.t Queue.t) ~file_id
  : (Typed_ast.Expr.t * Type.t, Grace.Diagnostic.t) result
  =
  let open Diagnostic in
  match expr.desc with
  | Literal l -> infer_literal l ~loc:expr.loc
  | Fun { params; body } ->
    infer_func ~params ~body ~env ~constraints ~file_id ~loc:expr.loc
  | Var var -> infer_var ~var ~env ~file_id ~loc:expr.loc
  | App { func; args } -> infer_app ~func ~args ~loc:expr.loc ~constraints ~env ~file_id
  | Let _ ->
    Error
      (error [%string "Unsupported let expression"]
       |> with_label ~file_id ~loc:expr.loc ~label:"specified here")
  | Letrec _ ->
    Error
      (error [%string "Unsupported letrec expression"]
       |> with_label ~file_id ~loc:expr.loc ~label:"specified here")
  | If _ ->
    Error
      (error [%string "Unsupported if expression"]
       |> with_label ~file_id ~loc:expr.loc ~label:"specified here")

and infer_func ~params ~body ~loc ~env ~constraints ~file_id =
  let open Result.Let_syntax in
  let env, params =
    List.fold_map params ~init:env ~f:(fun env (name, ty) ->
      let ty : Type.t =
        match ty with
        | None -> Var (Env.fresh_tv env)
        | Some ty -> Type.of_ast ty
      in
      Env.with_var env ~var:name.txt ~ty:(Type.Poly.mono ty), (name, ty))
  in
  let%bind body_typed_ast, body_type = infer' body ~env ~constraints ~file_id in
  Ok
    ( ({ desc = Fun { params; body = body_typed_ast; body_type }; loc } : Typed_ast.Expr.t)
    , (Fun (List.map params ~f:snd, body_type) : Type.t) )

and infer_app ~func ~args ~loc ~constraints ~env ~file_id =
  let open Result.Let_syntax in
  let%bind args =
    List.map args ~f:(fun arg -> infer' arg ~constraints ~env ~file_id) |> Result.all
  in
  let args, arg_tys = List.unzip args in
  let return_ty : Type.t = Var (Env.fresh_tv env) in
  let fun_ty : Type.t = Fun (arg_tys, return_ty) in
  let%bind fun_ast = check func fun_ty ~constraints ~env ~file_id in
  Ok (({ desc = App { func = fun_ast; args }; loc } : Typed_ast.Expr.t), return_ty)

and check (expr : Ast.Expr.t) expected_ty ~constraints ~env ~file_id =
  let open Result.Let_syntax in
  let%bind typed_ast, ty = infer' expr ~constraints ~env ~file_id in
  Queue.enqueue constraints (Same_type (ty, expected_ty));
  Ok typed_ast
;;

let infer (expr : Ast.Expr.t) ~env ~file_id =
  let open Result.Let_syntax in
  let constraints = Queue.create () in
  let%bind typed_expr, ty = infer' expr ~env ~constraints ~file_id in
  Ok (typed_expr, ty, Queue.to_list constraints)
;;

let type_ast (ast : Ast.t) ~file_id =
  let open Result.Let_syntax in
  List.fold_result ast ~init:(Env.empty ()) ~f:(fun env (top_level : Ast.Toplevel.t) ->
    match top_level with
    | External { loc = _; type_; name; c_name = _ } ->
      let env =
        Env.with_var env ~var:name.txt ~ty:(Type.Poly.mono (Type.of_ast type_.txt))
      in
      Ok env
    | Function { name; params; body; loc } ->
      let env =
        let tv = Env.fresh_tv env in
        Env.with_var env ~var:name.txt ~ty:(Type.Poly.mono (Var tv))
      in
      let%bind _typed_expr, ty, constraints =
        infer { desc = Fun { params; body }; loc } ~env ~file_id
      in
      print_endline (Type.to_string ty);
      print_s [%message (constraints : Constraint.t list)];
      Ok env)
;;
