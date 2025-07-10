open! Ox
open! Import

let defined (expr : Ast.Expr.t) ~env =
  match expr.desc with
  | Var v ->
    let%bind.Option defn = Env.var env v in
    Some defn.loc
  | _ -> None
;;

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
    let ty = Type.Poly.init ty.txt ~fresh_var:(fun () -> Env.fresh_tv env) in
    Ok ({ desc = Var var; loc }, ty)
;;

let rec infer' (expr : Ast.Expr.t) ~env ~(constraints : Constraint.t Queue.t) ~file_id
  : (Typed_ast.Expr.t * Type.t, Grace.Diagnostic.t) result
  =
  match expr.desc with
  | Literal l -> infer_literal l ~loc:expr.loc
  | Fun { params; body } ->
    infer_func ~params ~body ~env ~constraints ~file_id ~loc:expr.loc
  | Var var -> infer_var ~var ~env ~file_id ~loc:expr.loc
  | App { func; args } -> infer_app ~func ~args ~loc:expr.loc ~constraints ~env ~file_id
  | Letrec { var; type_; value; body } ->
    infer_let
      ~var
      ~type_
      ~value
      ~body
      ~loc:expr.loc
      ~constraints
      ~env
      ~file_id
      ~recursive:true
  | Let { var; type_; value; body } ->
    infer_let
      ~var
      ~type_
      ~value
      ~body
      ~loc:expr.loc
      ~constraints
      ~env
      ~file_id
      ~recursive:false
  | If { condition; if_true; if_false } ->
    infer_if ~condition ~if_true ~if_false ~loc:expr.loc ~constraints ~env ~file_id

and infer_if ~condition ~if_true ~if_false ~loc ~env ~constraints ~file_id =
  let open Result.Let_syntax in
  let%bind condition_expr, _condition_ty =
    check condition (Type.Base Bool) ~env ~constraints ~file_id
  in
  let%bind if_true, if_true_ty = infer' if_true ~env ~constraints ~file_id in
  let%bind if_false, _ = check if_false if_true_ty ~env ~constraints ~file_id in
  Ok
    ( ({ desc = If { condition = condition_expr; if_true; if_false }; loc }
       : Typed_ast.Expr.t)
    , if_true_ty )

and infer_let ~var ~type_ ~value ~body ~loc ~env ~constraints ~file_id ~recursive =
  (* [let v = x in y] should be equivalent to [(fun v -> y) x], but for the purposes
     of error messages, the two need to be slightly different. *)
  let open Result.Let_syntax in
  let%bind var_expr, var_ty =
    let env, type_ =
      if recursive
      then (
        (* FIXME: require that recursive values are functions *)
        let (var_ty, loc) : Type.t * Location.t =
          match type_ with
          | None -> Var (Env.fresh_tv env), var.loc
          | Some ty -> Type.of_ast ty.txt, ty.loc
        in
        ( Env.with_var env ~var:var.txt ~ty:{ txt = Type.Poly.mono var_ty; loc }
        , Some var_ty ))
      else env, Option.map type_ ~f:(fun type_ -> Type.of_ast type_.txt)
    in
    match type_ with
    | None -> infer' value ~env ~constraints ~file_id
    | Some type_ ->
      (* FIXME: it would be nice to add an annotation here like "defined here" *)
      check value type_ ~env ~constraints ~file_id
  in
  let env =
    Env.with_var env ~var:var.txt ~ty:{ txt = Type.Poly.mono var_ty; loc = var.loc }
  in
  let%bind body_expr, body_ty = infer' body ~env ~constraints ~file_id in
  Ok
    ( { Typed_ast.Expr.desc =
          Let { var; type_ = var_ty; value = var_expr; body = body_expr }
      ; loc
      }
    , body_ty )

and infer_func ~params ~body ~loc ~env ~constraints ~file_id =
  let open Result.Let_syntax in
  let env, params =
    List.fold_map params ~init:env ~f:(fun env (name, ty) ->
      let ty : Type.t =
        match ty with
        | None -> Var (Env.fresh_tv env)
        | Some ty -> Type.of_ast ty
      in
      ( Env.with_var env ~var:name.txt ~ty:{ txt = Type.Poly.mono ty; loc = name.loc }
      , (name, ty) ))
  in
  let%bind body_typed_ast, body_type = infer' body ~env ~constraints ~file_id in
  Ok
    ( ({ desc = Fun { params; body = body_typed_ast; body_type }; loc } : Typed_ast.Expr.t)
    , (Fun (List.map params ~f:snd, body_type) : Type.t) )

and infer_app ~func ~args ~loc ~constraints ~env ~file_id =
  let open Result.Let_syntax in
  (* type_of([$function_($arg_values...)])

       - [arg_values] is a list of expressions, so just infer the type of each expression.
       - Infer the type of [function_].
       - [check] that the inferred type of [function_] is [Fun _].

       We do this in two phases: first we generate fresh type variables for each argument to the
       function, and then we add constraints that check that each type variable is equal to the
       inferred type of each value in [arg_values]. This pushes type errors "down" into each
       argument, rather than checking the function as a whole, giving better error messages (at
       the cost of more constraints to iterate through). *)
  let fresh_arg_tys = List.map args ~f:(fun _ : Type.t -> Var (Env.fresh_tv env)) in
  let return_ty = Type.var (Env.fresh_tv env) in
  let fun_ty : Type.t = Fun (fresh_arg_tys, return_ty) in
  let%bind fun_ast, _ = check func fun_ty ~constraints ~env ~file_id in
  let%bind arg_asts =
    List.map2_exn args fresh_arg_tys ~f:(fun arg_value fresh_ty ->
      let%bind arg_ast, arg_ty = infer' arg_value ~constraints ~env ~file_id in
      Queue.enqueue
        constraints
        (Same_type
           ( arg_ty
           , fresh_ty
           , [ Expected_type
                 ( arg_value
                 , fresh_ty
                 , ~but:(Some arg_ty)
                 , ~defined:(defined arg_value ~env) )
             ; Expected_type (func, fun_ty, ~but:None, ~defined:(defined func ~env))
             ] ));
      Ok arg_ast)
    |> Result.all
  in
  Ok
    ( ({ desc = App { func = fun_ast; args = arg_asts }; loc } : Typed_ast.Expr.t)
    , return_ty )

and check (expr : Ast.Expr.t) expected_ty ~constraints ~env ~file_id =
  let open Result.Let_syntax in
  let%bind typed_ast, ty = infer' expr ~constraints ~env ~file_id in
  Queue.enqueue
    constraints
    (Same_type
       ( ty
       , expected_ty
       , [ Expected_type (expr, expected_ty, ~but:(Some ty), ~defined:(defined expr ~env))
         ] ));
  Ok (typed_ast, ty)
;;

let infer (expr : Ast.Expr.t) ~env ~file_id =
  let open Result.Let_syntax in
  let constraints = Queue.create () in
  let%bind typed_expr, ty = infer' expr ~env ~constraints ~file_id in
  Ok (typed_expr, ty, Queue.to_list constraints)
;;

let type_ast (ast : Ast.t) ~file_id : (Typed_ast.t, _) result =
  let open Result.Let_syntax in
  let env =
    Env.empty ()
    |> Env.with_var
         ~var:(Identifier.of_string "+")
         ~ty:
           { txt = Type.Poly.mono (Fun ([ Base I32; Base I32 ], Base I32))
           ; loc = Location.initial
           }
    |> Env.with_var
         ~var:(Identifier.of_string "=")
         ~ty:
           { txt = Type.Poly.mono (Fun ([ Base I32; Base I32 ], Base Bool))
           ; loc = Location.initial
           }
    |> Env.with_var
         ~var:(Identifier.of_string "-")
         ~ty:
           { txt = Type.Poly.mono (Fun ([ Base I32; Base I32 ], Base I32))
           ; loc = Location.initial
           }
  in
  let%bind _env, ast =
    List.fold_result
      ast
      ~init:(env, [])
      ~f:(fun (env, acc) (top_level : Ast.Toplevel.t) ->
        match top_level with
        | External { loc; type_; name; c_name } ->
          let type_ : _ Located.t =
            { txt = Type.Poly.mono (Type.of_ast type_.txt); loc = type_.loc }
          in
          let env = Env.with_var env ~var:name.txt ~ty:type_ in
          Ok (env, Typed_ast.Toplevel.External { loc; type_; name; c_name } :: acc)
        | Function { name; params; return_type; body; loc } ->
          let arg_types = List.map params ~f:(fun _ : Type.t -> Var (Env.fresh_tv env)) in
          let return_type_ty = 
            match return_type with
            | Some rt -> Type.of_ast rt.txt
            | None -> Var (Env.fresh_tv env)
          in
          let ty : Type.t = Fun (arg_types, return_type_ty) in
          let env =
            Env.with_var env ~var:name.txt ~ty:{ txt = Type.Poly.mono ty; loc = name.loc }
          in
          let constraints = Queue.create () in
          let%bind body, _ty =
            check { desc = Fun { params; body }; loc } ty ~env ~file_id ~constraints
          in
          let body =
            match body.desc with
            | Fun { params = _; body; body_type = _ } -> body
            | _ -> assert false
          in
          let constraints = Queue.to_list constraints in
          let solver = Solver.create () in
          let%bind env = Solver.solve solver constraints ~env ~file_id in
          let typed_return_type = Solver.normalize_ty solver return_type_ty ~env in
          Ok
            ( env
            , Typed_ast.Toplevel.Function
                { name
                ; params =
                    List.map2_exn params arg_types ~f:(fun (ident, _) ty ->
                      ident, Solver.normalize_ty solver ty ~env)
                ; return_type = typed_return_type
                ; body
                ; loc
                }
              :: acc ))
  in
  Ok (List.rev ast)
;;
