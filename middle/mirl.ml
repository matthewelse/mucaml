open! Ox
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
  type t =
    | I32
    | I64
  [@@deriving equal, sexp_of, to_string ~capitalize:"snake_case"]
end

module Virtual_register : sig
  type t : immediate [@@deriving compare, hash, sexp_of, to_string]

  include Hashable.S_plain with type t := t
  include Comparable.S_plain with type t := t
  include Intable.S with type t := t
end = struct
  type t = int [@@deriving compare, hash, sexp_of]

  include functor Hashable.Make_plain
  include functor Comparable.Make_plain

  let to_string t = [%string "$%{t#Int}"]
  let to_int_exn t = t

  let of_int_exn t =
    if t < 0 then raise_s [%message "Virtual register cannot be negative" (t : int)];
    t
  ;;
end

module Register_descriptor = struct
  type t = { ty : Type.t } [@@deriving sexp_of]
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
    | Add_with_carry of
        { dst : R.t
        ; src1 : R.t
        ; src2 : R.t
        }
    | Sub_with_carry of
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
    | Return of R.t list
    | Branch of
        { condition : R.t
        ; target : Label.t
        }
  [@@deriving sexp_of]

  let consumes t = exclave_
    match t with
    | Add { src1; src2; _ } -> [ src1; src2 ]
    | Sub { src1; src2; _ } -> [ src1; src2 ]
    | Add_with_carry { src1; src2; _ } -> [ src1; src2 ]
    | Sub_with_carry { src1; src2; _ } -> [ src1; src2 ]
    | Set _ -> []
    | Mov { src; _ } -> [ src ]
    | C_call { args; _ } -> args
    | Jump _ -> []
    | Return regs -> regs
    | Branch { condition; _ } -> [ condition ]
  ;;

  let produces t =
    match t with
    | Add { dst; _ }
    | Sub { dst; _ }
    | Add_with_carry { dst; _ }
    | Sub_with_carry { dst; _ }
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
    | Add_with_carry { dst; src1; src2 } ->
      [%string
        "%{dst#Virtual_register} := %{src1#Virtual_register} +c %{src2#Virtual_register}"]
    | Sub_with_carry { dst; src1; src2 } ->
      [%string
        "%{dst#Virtual_register} := %{src1#Virtual_register} -c %{src2#Virtual_register}"]
    | Set { dst; value } -> [%string "%{dst#Virtual_register} := %{value#Int}"]
    | Mov { dst; src } -> [%string "%{dst#Virtual_register} := %{src#Virtual_register}"]
    | C_call { dst; func; args } ->
      let args_str =
        String.concat ~sep:", " (List.map args ~f:(fun r -> Virtual_register.to_string r))
      in
      [%string "%{dst#Virtual_register} := c_call %{func}(%{args_str})"]
    | Jump { target } -> [%string "jump %{target#Label}"]
    | Return regs ->
      let regs_str =
        String.concat ~sep:", " (List.map regs ~f:Virtual_register.to_string)
      in
      [%string "return %{regs_str}"]
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
    ; params : (Identifier.t Located.t * Virtual_register.t * Type.t) list
    ; body : Block.t iarray
    ; registers : Register_descriptor.t iarray
    }
  [@@deriving sexp_of]

  module Builder = struct
    type t =
      { body : Block.Builder.t Queue.t @@ global
      ; registers : Register_descriptor.t Queue.t @@ global
      }

    let create () = { body = Queue.create (); registers = Queue.create () }
    let fresh_label t = Queue.length t.body |> Label.of_int_exn

    let fresh_register t ~ty =
      let reg = Virtual_register.of_int_exn (Queue.length t.registers) in
      Queue.enqueue t.registers { ty };
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

    let get_register_type (t @ local) reg =
      let index = Virtual_register.to_int_exn reg in
      if index < 0 || index >= Queue.length t.registers
      then failwith "Invalid register index";
      let%tydi { ty } = Queue.get t.registers index in
      ty
    ;;
  end

  let build ~name ~params (f : (_ @ local -> _ -> _) @ local) =
    let builder = Builder.create () in
    let params =
      List.map params ~f:(fun (p, ty) ->
        let reg = Builder.fresh_register builder ~ty in
        p, reg, ty)
    in
    f builder params;
    let body =
      Queue.to_array builder.body
      |> Array.map ~f:Block.Builder.finalize_exn
      |> Iarray.unsafe_of_array__promise_no_mutation
    in
    let registers =
      Queue.to_array builder.registers |> Iarray.unsafe_of_array__promise_no_mutation
    in
    { name; params; body; registers }
  ;;

  let to_string { name; params; body; registers } =
    let params_str =
      String.concat
        ~sep:", "
        (List.map params ~f:(fun (p, reg, t) ->
           [%string "%{reg#Virtual_register} (%{p.txt#Identifier}): %{t#Type}"]))
    in
    let body =
      Iarray.map ~f:(Block.to_string ~indent:"  ") body
      |> Iarray.to_list
      |> String.concat ~sep:"\n"
    in
    let registers =
      Iarray.mapi
        ~f:(fun i { ty } ->
          let reg = Virtual_register.of_int_exn i in
          [%string "%{reg#Virtual_register}: %{ty#Type}"])
        registers
      |> Iarray.to_list
      |> String.concat ~sep:", "
    in
    [%string "function %{name} (%{params_str}) {\n%{registers}\n%{body}\n}"]
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
  type t = Value.t Identifier.Map.t [@@deriving sexp_of]

  let empty = Identifier.Map.empty
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
      | Function { name; params; body; location = _ } ->
        let params =
          List.map params ~f:(fun (param, ty) ->
            let ty : Type.t =
              match ty with
              | Base I32 -> Type.I32
              | Base I64 -> Type.I64
              | _ -> failwith "todo: unsupported type"
            in
            param, ty)
        in
        let name = [%string "mucaml_%{name.txt#Identifier}"] in
        First
          (Function.build ~name ~params (fun builder params ->
             let env =
               List.fold
                 params
                 ~init:!env
                 ~f:(fun env ({ Located.txt = name; loc = _ }, reg, _) ->
                   Map.set env ~key:name ~data:(Value.Virtual_register reg))
             in
             Function.Builder.add_block' builder (fun acc ->
               let #(result, acc) @ local =
                 walk_expr body ~function_builder:builder ~env ~acc
               in
               Block.Builder.push acc (Instruction.Return [ result ]) [@nontail])
             [@nontail]))
      | External { name; type_; c_name; location = _ } ->
        let arg_types, return_type =
          let rec args acc (ret : Mucaml_frontend.Type.t) =
            match ret with
            | Fun (Base I32, ret) -> args (Type.I32 :: acc) ret
            | Fun (Base I64, ret) -> args (Type.I64 :: acc) ret
            | Fun ((Base _ | Fun _), _) -> failwith "todo: unsupported type"
            | Base (Unit | I32) -> List.rev acc, Type.I32
            | Base I64 -> List.rev acc, Type.I64
            | Base Bool -> failwith "todo: bools"
          in
          args [] type_.txt
        in
        env := Map.set !env ~key:name.txt ~data:(Value.Global c_name.txt);
        Second
          { External.name = Identifier.to_string name.txt
          ; arg_types
          ; return_type
          ; c_name = c_name.txt
          })
  in
  { functions; externs }

and walk_expr
  (expr : Ast.Expr.t)
  ~(env : Env.t)
  ~(function_builder @ local)
  ~(acc : Block.Builder.t @ local)
  : #(Virtual_register.t * Block.Builder.t) @ local
  = exclave_
  let open Block.Builder in
  let require_type_equal reg1 reg2 =
    let ty1 = Function.Builder.get_register_type function_builder reg1 in
    let ty2 = Function.Builder.get_register_type function_builder reg2 in
    if not (Type.equal ty1 ty2)
    then
      raise_s
        [%message
          "Expected registers to have the same type"
            (reg1 : Virtual_register.t)
            (ty1 : Type.t)
            (reg2 : Virtual_register.t)
            (ty2 : Type.t)];
    ty1
  in
  match expr with
  | { desc = Literal (Int32 i); _ } ->
    let reg = Function.Builder.fresh_register function_builder ~ty:I32 in
    push acc (Set { dst = reg; value = I32.to_int_trunc i });
    #(reg, acc)
  | { desc = Literal (Int64 i); _ } ->
    let reg = Function.Builder.fresh_register function_builder ~ty:I64 in
    push acc (Set { dst = reg; value = I64.to_int_trunc i });
    #(reg, acc)
  | { desc = App { func = { desc = Var op; _ }; args = [ e1; e2 ] }; _ }
    when String.equal (Identifier.to_string op) "+" ->
    let #(reg1, acc) = walk_expr e1 ~env ~function_builder ~acc in
    let #(reg2, acc) = walk_expr e2 ~env ~function_builder ~acc in
    let ty = require_type_equal reg1 reg2 in
    let dst = Function.Builder.fresh_register function_builder ~ty in
    let add_ins : Instruction.t = Add { dst; src1 = reg1; src2 = reg2 } in
    push acc add_ins;
    #(dst, acc)
  | { desc = App { func = { desc = Var op; _ }; args = [ e1; e2 ] }; _ }
    when String.equal (Identifier.to_string op) "-" ->
    let #(reg1, acc) = walk_expr e1 ~env ~function_builder ~acc in
    let #(reg2, acc) = walk_expr e2 ~env ~function_builder ~acc in
    let ty = require_type_equal reg1 reg2 in
    let dst = Function.Builder.fresh_register function_builder ~ty in
    let sub_ins : Instruction.t = Sub { dst; src1 = reg1; src2 = reg2 } in
    push acc sub_ins;
    #(dst, acc)
  | { desc = Let { var; type_ = _; value; body }; _ } ->
    let #(reg1, acc) = walk_expr value ~env ~function_builder ~acc in
    let env = Map.set env ~key:var.txt ~data:(Value.Virtual_register reg1) in
    let #(reg2, acc) = walk_expr body ~env ~function_builder ~acc in
    #(reg2, acc)
  | { desc = App { func = { desc = Var var; _ }; args }; _ } ->
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
       (* FIXME: handle different return types *)
       let dst = Function.Builder.fresh_register function_builder ~ty:I32 in
       let c_call_ins : Instruction.t = C_call { dst; func = c_name; args } in
       push acc c_call_ins;
       #(dst, acc))
  | { desc = Var name; _ } ->
    let value = Map.find_exn env name in
    (match value with
     | Virtual_register reg -> #(reg, acc)
     | Global _ ->
       (match failwith "todo: global variables in expressions" with
        | (_ : Nothing.t) -> .))
  | { desc = If { condition; if_true; if_false }; _ } ->
    let #(cond_reg, acc) = walk_expr condition ~env ~function_builder ~acc in
    let then_block = Function.Builder.add_block function_builder ignore in
    let else_block = Function.Builder.add_block function_builder ignore in
    let after_block = Function.Builder.add_block function_builder ignore in
    let #(then_reg, then_block) =
      let acc = then_block in
      walk_expr if_true ~env ~function_builder ~acc
    in
    let #(else_reg, else_block) =
      let acc = else_block in
      walk_expr if_false ~env ~function_builder ~acc
    in
    let ty = require_type_equal then_reg else_reg in
    let dst = Function.Builder.fresh_register function_builder ~ty in
    let () =
      let acc = then_block in
      push acc (Mov { dst; src = then_reg });
      Block.Builder.register_successor acc after_block.label;
      push acc (Jump { target = after_block.label })
    in
    let () =
      let acc = else_block in
      push acc (Mov { dst; src = else_reg });
      Block.Builder.register_successor acc after_block.label;
      push acc (Jump { target = after_block.label })
    in
    Block.Builder.register_successor acc then_block.label;
    push acc (Branch { condition = cond_reg; target = then_block.label });
    Block.Builder.register_successor acc else_block.label;
    push acc (Jump { target = else_block.label });
    #(dst, after_block)
  | _ ->
    (match raise_s [%message "todo:" (expr : Ast.Expr.t)] with
     | (_ : Nothing.t) -> .)
;;
