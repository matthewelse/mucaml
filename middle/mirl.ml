open! Core
open! Import

module Label = struct
  include Unique_id.Int ()

  let to_string t = [%string "block_%{to_string t}"]

  let of_string s =
    match String.chop_prefix s ~prefix:"block_" with
    | Some label -> of_string label
    | None -> failwithf "Invalid label format: %s" s ()
  ;;

  include functor Sexpable.Of_stringable

  module For_testing = struct
    include For_testing

    let dummy = of_int_exn 0
  end
end

module Type = struct
  type t = Int32 [@@deriving sexp_of]

  let to_string = function
    | Int32 -> "int32"
  ;;
end

module Virtual_register = struct
  include Unique_id.Int ()

  let to_string t = [%string "$%{to_string t}"]
end

module R = Virtual_register

module Instruction = struct
  type t =
    | Add of
        { dst : R.t
        ; src1 : R.t
        ; src2 : R.t
        }
    | Sub of
        { dst : R.t
        ; src1 : R.t
        ; src2 : R.t
        }
    | Set of
        { dst : R.t
        ; value : int
        }
    | Mov of
        { dst : R.t
        ; src : R.t
        }
    | C_call of
        { dst : R.t
        ; func : string
        ; args : R.t list
        }
    | Jump of { target : Label.t }
    | Return of R.t
    | Branch of
        { condition : R.t
        ; target : Label.t
        }
  [@@deriving sexp_of]

  let to_string = function
    | Add { dst; src1; src2 } ->
      [%string
        "%{dst#Virtual_register} := %{src1#Virtual_register} + %{src2#Virtual_register}"]
    | Sub { dst; src1; src2 } ->
      [%string
        "%{dst#Virtual_register} := %{src1#Virtual_register} - %{src2#Virtual_register}"]
    | Set { dst; value } -> [%string "%{dst#Virtual_register} := %{value#Int}"]
    | Mov { dst; src } -> [%string "%{dst#Virtual_register} := %{src#Virtual_register}"]
    | C_call { dst; func; args } ->
      let args_str =
        String.concat ~sep:", " (List.map args ~f:(fun r -> Virtual_register.to_string r))
      in
      [%string "%{dst#Virtual_register} := c_call %{func}(%{args_str})"]
    | Jump { target } -> [%string "jump %{target#Label}"]
    | Return reg -> [%string "return %{reg#Virtual_register}"]
    | Branch { condition; target } ->
      [%string "branch if %{condition#Virtual_register} to %{target#Label}"]
  ;;
end

module Block = struct
  type t =
    { label : Label.t
    ; instructions : Instruction.t iarray
    }
  [@@deriving sexp_of]

  module Builder = struct
    type built = t

    type t =
      { label : Label.t
      ; instructions : Instruction.t Queue.t
      }

    let push t instruction = Queue.enqueue t.instructions instruction
    let create () = { label = Label.create (); instructions = Queue.create () }

    let finalize_exn t : built =
      if Queue.is_empty t.instructions then failwith "Cannot finalize an empty block";
      let label = t.label in
      let instructions =
        Queue.to_array t.instructions |> Iarray.unsafe_of_array__promise_no_mutation
      in
      { label; instructions }
    ;;
  end

  let to_string ?(indent = "") { label; instructions } =
    let indent = indent ^ "  " in
    Label.to_string label
    ^ ":\n"
    ^ String.concat
        ~sep:"\n"
        (List.map (Iarray.to_list instructions) ~f:(fun instruction ->
           indent ^ Instruction.to_string instruction))
  ;;
end

module Function = struct
  type t =
    { name : string
    ; params : (string * Virtual_register.t * Type.t) list
    ; body : Block.t iarray
    }
  [@@deriving sexp_of]

  module Builder = struct
    type t = { body : Block.Builder.t Queue.t }

    let create () = { body = Queue.create () }

    let add_block t f =
      let block = Block.Builder.create () in
      Queue.enqueue t.body block;
      f block;
      block
    ;;

    let add_block' t f = add_block t f |> (ignore : Block.Builder.t -> unit)
  end

  let build ~name ~params f =
    let builder = Builder.create () in
    f builder;
    let body =
      Queue.to_array builder.body
      |> Array.map ~f:Block.Builder.finalize_exn
      |> Iarray.unsafe_of_array__promise_no_mutation
    in
    { name; params; body }
  ;;

  let to_string { name; params; body } =
    let params_str =
      String.concat
        ~sep:", "
        (List.map params ~f:(fun (p, reg, t) ->
           [%string "%{reg#Virtual_register} (%{p}): %{t#Type}"]))
    in
    let body =
      Iarray.map ~f:(Block.to_string ~indent:"  ") body
      |> Iarray.to_list
      |> String.concat ~sep:"\n"
    in
    [%string "function %{name} (%{params_str}) {\n%{body}\n}"]
  ;;
end

module External = struct
  type t =
    { name : string
    ; arg_types : Type.t list
    ; return_type : Type.t
    ; c_name : string
    }
  [@@deriving sexp_of]

  let to_string { name; arg_types; return_type; c_name } =
    let type_ = List.map arg_types ~f:Type.to_string |> String.concat ~sep:" -> " in
    [%string "external %{name} : %{type_} -> %{return_type#Type} = \"%{c_name}\""]
  ;;
end

type t =
  { functions : Function.t list
  ; externs : External.t list
  }
[@@deriving sexp_of]

module Value = struct
  type t =
    | Virtual_register of Virtual_register.t
    | Global of string
  [@@deriving sexp_of]
end

module Env = struct
  type t = Value.t String.Map.t [@@deriving sexp_of]

  let empty = String.Map.empty
end

let to_string { functions; externs } =
  String.concat ~sep:"\n\n" (List.map functions ~f:Function.to_string)
  ^ "\n\n"
  ^ String.concat ~sep:"\n\n" (List.map externs ~f:External.to_string)
;;

let rec of_ast (ast : Ast.t) =
  let env = ref Env.empty in
  let functions, externs =
    List.partition_mapi ast ~f:(fun _ ast ->
      match ast with
      | Function { name; params; body } ->
        let params =
          List.map params ~f:(fun (param, ty) ->
            let ty : Type.t =
              match ty with
              | Int32 -> Type.Int32
              | _ -> failwith "todo: unsupported type"
            in
            let reg = Virtual_register.create () in
            param, reg, ty)
        in
        let env =
          List.fold params ~init:!env ~f:(fun env (name, reg, _) ->
            Map.set env ~key:name ~data:(Value.Virtual_register reg))
        in
        let name = [%string "mucaml_%{name}"] in
        First
          (Function.build ~name ~params (fun builder ->
             Function.Builder.add_block' builder (fun acc ->
               let #(result, acc) = walk_expr body ~function_builder:builder ~env ~acc in
               Block.Builder.push acc (Instruction.Return result))))
      | External { name; type_; c_name } ->
        let arg_types, return_type =
          let rec args acc (ret : Mucaml_frontend.Type.t) =
            match ret with
            | Fun (Int32, ret) -> args (Type.Int32 :: acc) ret
            | Fun ((Bool | Unit | Fun _), _) -> failwith "todo: unsupported type"
            | Unit | Int32 -> List.rev acc, Type.Int32
            | Bool -> failwith "todo: bools"
          in
          args [] type_
        in
        env := Map.set !env ~key:name ~data:(Value.Global c_name);
        Second { External.name; arg_types; return_type; c_name })
  in
  { functions; externs }

and walk_expr (expr : Ast.expr) ~(env : Env.t) ~function_builder ~(acc : Block.Builder.t)
  : #(Virtual_register.t * Block.Builder.t)
  =
  let open Block.Builder in
  match expr with
  | Int i ->
    let reg = Virtual_register.create () in
    push acc (Set { dst = reg; value = i });
    #(reg, acc)
  | App (Var "+", [ e1; e2 ]) ->
    let #(reg1, acc) = walk_expr e1 ~env ~function_builder ~acc in
    let #(reg2, acc) = walk_expr e2 ~env ~function_builder ~acc in
    let dst = Virtual_register.create () in
    let add_ins : Instruction.t = Add { dst; src1 = reg1; src2 = reg2 } in
    push acc add_ins;
    #(dst, acc)
  | App (Var "-", [ e1; e2 ]) ->
    let #(reg1, acc) = walk_expr e1 ~env ~function_builder ~acc in
    let #(reg2, acc) = walk_expr e2 ~env ~function_builder ~acc in
    let dst = Virtual_register.create () in
    let sub_ins : Instruction.t = Sub { dst; src1 = reg1; src2 = reg2 } in
    push acc sub_ins;
    #(dst, acc)
  | Let ((var, _), value, body) ->
    let #(reg1, acc) = walk_expr value ~env ~function_builder ~acc in
    let env = Map.set env ~key:var ~data:(Value.Virtual_register reg1) in
    let #(reg2, acc) = walk_expr body ~env ~function_builder ~acc in
    #(reg2, acc)
  | App (Var var, args) ->
    let acc, args =
      List.fold_map args ~init:acc ~f:(fun acc arg ->
        let #(reg, acc) = walk_expr arg ~env ~function_builder ~acc in
        acc, reg)
    in
    let func = Map.find_exn env var in
    (match func with
     | Virtual_register _ ->
       (match failwith "todo: indirect function calls" with
        | (_ : Nothing.t) -> .)
     | Global c_name ->
       let dst = Virtual_register.create () in
       let c_call_ins : Instruction.t = C_call { dst; func = c_name; args } in
       push acc c_call_ins;
       #(dst, acc))
  | Var name ->
    let value = Map.find_exn env name in
    (match value with
     | Virtual_register reg -> #(reg, acc)
     | Global _ ->
       (match failwith "todo: global variables in expressions" with
        | (_ : Nothing.t) -> .))
  | If (cond, if_true, if_false) ->
    let #(cond_reg, acc) = walk_expr cond ~env ~function_builder ~acc in
    let dst = Virtual_register.create () in
    let after_block = Function.Builder.add_block function_builder ignore in
    let then_block =
      Function.Builder.add_block function_builder (fun acc ->
        let #(then_reg, acc) = walk_expr if_true ~env ~function_builder ~acc in
        push acc (Mov { dst; src = then_reg });
        push acc (Jump { target = after_block.label }))
    in
    let else_block =
      Function.Builder.add_block function_builder (fun acc ->
        let #(else_reg, acc) = walk_expr if_false ~env ~function_builder ~acc in
        push acc (Mov { dst; src = else_reg });
        push acc (Jump { target = after_block.label }))
    in
    push acc (Branch { condition = cond_reg; target = then_block.label });
    push acc (Jump { target = else_block.label });
    #(dst, after_block)
  | _ ->
    (match raise_s [%message "todo:" (expr : Ast.expr)] with
     | (_ : Nothing.t) -> .)
;;
