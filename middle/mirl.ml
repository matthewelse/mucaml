(** [Mirl] is "matt's intermediate representation language", a low-level representation of
    the program that is closer to assembly than the original source code. It is used as an
    intermediate step before generating machine code. *)

open! Core
open! Import

module Type = struct
  type t = Int32 [@@deriving sexp_of]

  let to_string = function
    | Int32 -> "int32"
  ;;
end

module Register = struct
  include Unique_id.Int ()

  let to_string t = [%string "$%{to_string t}"]
end

module Instruction = struct
  type t =
    | Add of
        { dest : Register.t
        ; src1 : Register.t
        ; src2 : Register.t
        }
    | Sub of
        { dest : Register.t
        ; src1 : Register.t
        ; src2 : Register.t
        }
    | Set of
        { dest : Register.t
        ; value : int
        }
    | Mov of
        { dest : Register.t
        ; src : Register.t
        }
    | C_call of
        { dest : Register.t
        ; func : string
        ; args : Register.t list
        }
  [@@deriving sexp_of]

  let to_string = function
    | Add { dest; src1; src2 } ->
      [%string "%{dest#Register} := %{src1#Register} + %{src2#Register}"]
    | Sub { dest; src1; src2 } ->
      [%string "%{dest#Register} := %{src1#Register} - %{src2#Register}"]
    | Set { dest; value } -> [%string "%{dest#Register} := %{value#Int}"]
    | Mov { dest; src } -> [%string "%{dest#Register} := %{src#Register}"]
    | C_call { dest; func; args } ->
      let args_str =
        String.concat ~sep:", " (List.map args ~f:(fun r -> Register.to_string r))
      in
      [%string "%{dest#Register} := c_call %{func}(%{args_str})"]
  ;;
end

module Terminator = struct
  type t = Return of Register.t [@@deriving sexp_of]

  let to_string = function
    | Return reg -> [%string "return %{reg#Register}"]
  ;;
end

module Block = struct
  type t =
    { instructions : Instruction.t iarray
    ; terminator : Terminator.t
    }
  [@@deriving sexp_of]

  let to_string ?(indent = "") { instructions; terminator } =
    String.concat
      ~sep:"\n"
      (List.map (Iarray.to_list instructions) ~f:(fun instruction ->
         indent ^ Instruction.to_string instruction))
    ^ "\n"
    ^ indent
    ^ Terminator.to_string terminator
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
    | Register of Register.t
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
    List.partition_map ast ~f:(fun ast ->
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
        let acc = Queue.to_array acc |> Iarray.unsafe_of_array__promise_no_mutation in
        let block = { Block.instructions = acc; terminator = Return result } in
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

and walk_expr (expr : Ast.expr) ~(env : Env.t) ~(acc : Instruction.t Queue.t) : Register.t
  =
  match expr with
  | Int i ->
    let reg = Register.create () in
    Queue.enqueue acc (Set { dest = reg; value = i });
    reg
  | App (Var "+", [ e1; e2 ]) ->
    let reg1 = walk_expr e1 ~env ~acc in
    let reg2 = walk_expr e2 ~env ~acc in
    let dest = Register.create () in
    let add_ins : Instruction.t = Add { dest; src1 = reg1; src2 = reg2 } in
    Queue.enqueue acc add_ins;
    dest
  | App (Var "-", [ e1; e2 ]) ->
    let reg1 = walk_expr e1 ~env ~acc in
    let reg2 = walk_expr e2 ~env ~acc in
    let dest = Register.create () in
    let sub_ins : Instruction.t = Sub { dest; src1 = reg1; src2 = reg2 } in
    Queue.enqueue acc sub_ins;
    dest
  | Let ((var, _), value, body) ->
    let reg1 = walk_expr value ~env ~acc in
    let env = Map.set env ~key:var ~data:(Value.Register reg1) in
    let reg2 = walk_expr body ~env ~acc in
    reg2
  | App (Var var, args) ->
    let args = List.map args ~f:(fun arg -> walk_expr arg ~env ~acc) in
    let func = Map.find_exn env var in
    (match func with
     | Register _ -> failwith "todo: indirect function calls"
     | Global c_name ->
       let dest = Register.create () in
       let c_call_ins : Instruction.t = C_call { dest; func = c_name; args } in
       Queue.enqueue acc c_call_ins;
       dest)
  | Var name ->
    let value = Map.find_exn env name in
    (match value with
     | Register reg -> reg
     | Global _ -> failwith "todo: global variables in expressions")
  | _ -> raise_s [%message "todo:" (expr : Ast.expr)]
;;
