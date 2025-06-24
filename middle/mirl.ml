open! Core
open! Import

module Label = struct
  type t = int

  let to_string t = [%string "block_%{t#Int}"]

  let of_string s =
    match String.chop_prefix s ~prefix:"block_" with
    | Some label -> Int.of_string label
    | None -> failwithf "Invalid label format: %s" s ()
  ;;

  include functor Sexpable.Of_stringable

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
    | Jump { target } -> [%string "jump %{target#Int}"]
    | Return reg -> [%string "return %{reg#Virtual_register}"]
    | Branch { condition; target } ->
      [%string "branch if %{condition#Virtual_register} to %{target#Int}"]
  ;;
end

module Block = struct
  type t =
    { label : Label.t
    ; instructions : Instruction.t iarray
    }
  [@@deriving sexp_of]

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
    ; params : (string * Type.t) list
    ; body : Block.t
    }
  [@@deriving sexp_of]

  let to_string { name; params; body } =
    let params_str =
      String.concat
        ~sep:", "
        (List.map params ~f:(fun (p, t) -> [%string "%{p}: %{t#Type}"]))
    in
    let body = Block.to_string ~indent:"  " body in
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
    List.partition_mapi ast ~f:(fun i ast ->
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
        let acc = Queue.create () in
        let result = walk_expr body ~env:!env ~acc in
        Queue.enqueue acc (Instruction.Return result);
        let instructions =
          Queue.to_array acc |> Iarray.unsafe_of_array__promise_no_mutation
        in
        let block : Block.t = { instructions; label = i } in
        First { Function.name = [%string "mucaml_%{name}"]; params; body = block }
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

and walk_expr (expr : Ast.expr) ~(env : Env.t) ~(acc : Instruction.t Queue.t)
  : Virtual_register.t
  =
  match expr with
  | Int i ->
    let reg = Virtual_register.create () in
    Queue.enqueue acc (Set { dst = reg; value = i });
    reg
  | App (Var "+", [ e1; e2 ]) ->
    let reg1 = walk_expr e1 ~env ~acc in
    let reg2 = walk_expr e2 ~env ~acc in
    let dst = Virtual_register.create () in
    let add_ins : Instruction.t = Add { dst; src1 = reg1; src2 = reg2 } in
    Queue.enqueue acc add_ins;
    dst
  | App (Var "-", [ e1; e2 ]) ->
    let reg1 = walk_expr e1 ~env ~acc in
    let reg2 = walk_expr e2 ~env ~acc in
    let dst = Virtual_register.create () in
    let sub_ins : Instruction.t = Sub { dst; src1 = reg1; src2 = reg2 } in
    Queue.enqueue acc sub_ins;
    dst
  | Let ((var, _), value, body) ->
    let reg1 = walk_expr value ~env ~acc in
    let env = Map.set env ~key:var ~data:(Value.Virtual_register reg1) in
    let reg2 = walk_expr body ~env ~acc in
    reg2
  | App (Var var, args) ->
    let args = List.map args ~f:(fun arg -> walk_expr arg ~env ~acc) in
    let func = Map.find_exn env var in
    (match func with
     | Virtual_register _ -> failwith "todo: indirect function calls"
     | Global c_name ->
       let dst = Virtual_register.create () in
       let c_call_ins : Instruction.t = C_call { dst; func = c_name; args } in
       Queue.enqueue acc c_call_ins;
       dst)
  | Var name ->
    let value = Map.find_exn env name in
    (match value with
     | Virtual_register reg -> reg
     | Global _ -> failwith "todo: global variables in expressions")
  | _ -> raise_s [%message "todo:" (expr : Ast.expr)]
;;
