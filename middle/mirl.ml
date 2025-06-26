open! Core
open! Import

module Label : sig
  type t : immediate [@@deriving equal, sexp_of, to_string]

  module For_testing : sig
    val dummy : t
  end

  val of_int_exn : int -> t
  val to_int_exn : t -> int
end = struct
  type t = int [@@deriving equal]

  let to_string t = [%string "block_%{t#Int}"]
  let sexp_of_t t = Sexp.Atom (to_string t)

  let of_int_exn t =
    if t < 0 then raise_s [%message "Label cannot be negative" (t : int)];
    t
  ;;

  let to_int_exn t = t

  module For_testing = struct
    let dummy = 0
  end
end

module Type = struct
  type t = Int32 [@@deriving sexp_of]

  let to_string = function
    | Int32 -> "int32"
  ;;
end

module Virtual_register : sig
  type t : immediate [@@deriving compare, hash, sexp_of, to_string]

  include Hashable.S_plain with type t := t
  include Comparable.S_plain with type t := t
  include Intable.S with type t := t

  val zero : t
  val succ : t -> t
end = struct
  type t = int [@@deriving compare, hash, sexp_of]

  include functor Hashable.Make_plain
  include functor Comparable.Make_plain

  let to_string t = [%string "$%{t#Int}"]
  let zero = 0
  let succ t = t + 1
  let to_int_exn t = t

  let of_int_exn t =
    if t < 0 then raise_s [%message "Virtual register cannot be negative" (t : int)];
    t
  ;;
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

  let consumes t = exclave_
    match t with
    | Add { src1; src2; _ } -> [ src1; src2 ]
    | Sub { src1; src2; _ } -> [ src1; src2 ]
    | Set _ -> []
    | Mov { src; _ } -> [ src ]
    | C_call { args; _ } -> args
    | Jump _ -> []
    | Return reg -> [ reg ]
    | Branch { condition; _ } -> [ condition ]
  ;;

  let produces t =
    match t with
    | Add { dst; _ }
    | Sub { dst; _ }
    | Set { dst; _ }
    | Mov { dst; _ }
    | C_call { dst; _ } -> exclave_ Some dst
    | Jump _ -> None
    | Return _ -> None
    | Branch _ -> None
  ;;

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
    ; successors : Label.t iarray
    }
  [@@deriving sexp_of]

  module Builder = struct
    type built = t

    type t =
      { label : Label.t
      ; instructions : Instruction.t Queue.t @@ global
      ; successors : Label.t Queue.t @@ global
      }
    [@@deriving fields ~getters]

    let push (t @ local) instruction = Queue.enqueue t.instructions instruction
    let push_many (t @ local) instructions = Queue.enqueue_all t.instructions instructions

    let create label =
      { label; instructions = Queue.create (); successors = Queue.create () }
    ;;

    let register_successor (t @ local) label = Queue.enqueue t.successors label

    let finalize_exn t : built =
      if Queue.is_empty t.instructions then failwith "Cannot finalize an empty block";
      let label = t.label in
      let instructions =
        Queue.to_array t.instructions |> Iarray.unsafe_of_array__promise_no_mutation
      in
      let successors =
        Queue.to_array t.successors |> Iarray.unsafe_of_array__promise_no_mutation
      in
      { label; instructions; successors }
    ;;
  end

  let to_string ?(indent = "") { label; instructions; successors = _ } =
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
    type t =
      { body : Block.Builder.t Queue.t @@ global
      ; mutable next_register : Virtual_register.t
      }

    let create () = { body = Queue.create (); next_register = Virtual_register.zero }
    let fresh_label t = Queue.length t.body |> Label.of_int_exn

    let fresh_register t =
      let reg = t.next_register in
      t.next_register <- Virtual_register.succ t.next_register;
      reg
    ;;

    let add_block (t @ local) (f : (_ @ local -> unit) @ local) =
      let label = fresh_label t in
      let block = Block.Builder.create label in
      Queue.enqueue t.body block;
      f block;
      block
    ;;

    let add_block' t f = add_block t f |> (ignore : Block.Builder.t -> unit)
  end

  let build ~name ~params (f : (_ @ local -> _ -> _) @ local) =
    let builder = Builder.create () in
    let params =
      List.map params ~f:(fun (p, ty) ->
        let reg = Builder.fresh_register builder in
        p, reg, ty)
    in
    f builder params;
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
            param, ty)
        in
        let name = [%string "mucaml_%{name}"] in
        First
          (Function.build ~name ~params (fun builder params ->
             let env =
               List.fold params ~init:!env ~f:(fun env (name, reg, _) ->
                 Map.set env ~key:name ~data:(Value.Virtual_register reg))
             in
             Function.Builder.add_block' builder (fun acc ->
               let #(result, acc) @ local =
                 walk_expr body ~function_builder:builder ~env ~acc
               in
               Block.Builder.push acc (Instruction.Return result) [@nontail])
             [@nontail]))
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

and walk_expr
  (expr : Ast.expr)
  ~(env : Env.t)
  ~(function_builder @ local)
  ~(acc : Block.Builder.t @ local)
  : #(Virtual_register.t * Block.Builder.t) @ local
  = exclave_
  let open Block.Builder in
  match expr with
  | Int i ->
    let reg = Function.Builder.fresh_register function_builder in
    push acc (Set { dst = reg; value = i });
    #(reg, acc)
  | App (Var "+", [ e1; e2 ]) ->
    let #(reg1, acc) = walk_expr e1 ~env ~function_builder ~acc in
    let #(reg2, acc) = walk_expr e2 ~env ~function_builder ~acc in
    let dst = Function.Builder.fresh_register function_builder in
    let add_ins : Instruction.t = Add { dst; src1 = reg1; src2 = reg2 } in
    push acc add_ins;
    #(dst, acc)
  | App (Var "-", [ e1; e2 ]) ->
    let #(reg1, acc) = walk_expr e1 ~env ~function_builder ~acc in
    let #(reg2, acc) = walk_expr e2 ~env ~function_builder ~acc in
    let dst = Function.Builder.fresh_register function_builder in
    let sub_ins : Instruction.t = Sub { dst; src1 = reg1; src2 = reg2 } in
    push acc sub_ins;
    #(dst, acc)
  | Let ((var, _), value, body) ->
    let #(reg1, acc) = walk_expr value ~env ~function_builder ~acc in
    let env = Map.set env ~key:var ~data:(Value.Virtual_register reg1) in
    let #(reg2, acc) = walk_expr body ~env ~function_builder ~acc in
    #(reg2, acc)
  | App (Var var, args) ->
    let rec fold_map__local_acc ~init:(acc @ local) ~f = function
      | [] -> exclave_ acc, { global = [] }
      | x :: xs ->
        exclave_
        let #(acc, y) @ local = f acc x in
        let acc, ys = fold_map__local_acc ~init:acc ~f xs in
        acc, { global = y.global :: ys.global }
    in
    let acc, { global = args } =
      fold_map__local_acc args ~init:acc ~f:(fun acc arg -> exclave_
        let #(arg_reg, acc) = walk_expr arg ~env ~function_builder ~acc in
        #(acc, { global = arg_reg }))
    in
    let func = Map.find_exn env var in
    (match func with
     | Virtual_register _ ->
       (match failwith "todo: indirect function calls" with
        | (_ : Nothing.t) -> .)
     | Global c_name ->
       let dst = Function.Builder.fresh_register function_builder in
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
    let dst = Function.Builder.fresh_register function_builder in
    let after_block = Function.Builder.add_block function_builder ignore in
    let then_block =
      Function.Builder.add_block function_builder (fun acc ->
        let #(then_reg, acc) = walk_expr if_true ~env ~function_builder ~acc in
        push acc (Mov { dst; src = then_reg });
        Block.Builder.register_successor acc after_block.label;
        push acc (Jump { target = after_block.label }) [@nontail])
    in
    let else_block =
      Function.Builder.add_block function_builder (fun acc ->
        let #(else_reg, acc) = walk_expr if_false ~env ~function_builder ~acc in
        push acc (Mov { dst; src = else_reg });
        Block.Builder.register_successor acc after_block.label;
        push acc (Jump { target = after_block.label }) [@nontail])
    in
    Block.Builder.register_successor acc then_block.label;
    push acc (Branch { condition = cond_reg; target = then_block.label });
    Block.Builder.register_successor acc else_block.label;
    push acc (Jump { target = else_block.label });
    #(dst, after_block)
  | _ ->
    (match raise_s [%message "todo:" (expr : Ast.expr)] with
     | (_ : Nothing.t) -> .)
;;
