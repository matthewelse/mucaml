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
    | Set of
        { dest : Register.t
        ; value : int
        }
    | Return of Register.t
  [@@deriving sexp_of]

  let to_string = function
    | Add { dest; src1; src2 } ->
      [%string "%{dest#Register} := %{src1#Register} + %{src2#Register}"]
    | Set { dest; value } -> [%string "%{dest#Register} := %{value#Int}"]
    | Return reg -> [%string "return %{reg#Register}"]
  ;;
end

module Block = struct
  type t = { instructions : Instruction.t list } [@@deriving sexp_of]

  let to_string ?(indent = "") { instructions } =
    String.concat
      ~sep:"\n"
      (List.map instructions ~f:(fun instruction ->
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

type t = { functions : Function.t list } [@@deriving sexp_of]

let to_string { functions } =
  String.concat ~sep:"\n\n" (List.map functions ~f:Function.to_string)
;;

let rec of_ast (ast : Ast.t) =
  match ast with
  | Function { name; params; body } ->
    let params =
      List.map params ~f:(fun (param, ty) ->
        let ty : Type.t =
          match ty with
          | Int32 -> Type.Int32
          | _ -> failwith "Unsupported type"
        in
        param, ty)
    in
    let body, ~result = walk_expr body in
    let instructions = body @ [ Instruction.Return result ] in
    let block = { Block.instructions } in
    let function_ =
      { Function.name = [%string "mucaml_%{name}"]; params; body = block }
    in
    { functions = [ function_ ] }

and walk_expr (expr : Ast.expr) : Instruction.t list * result:Register.t =
  match expr with
  | Int i ->
    let reg = Register.create () in
    [ Instruction.Set { dest = reg; value = i } ], ~result:reg
  | App (Var "+", [ e1; e2 ]) ->
    let ins1, ~result:reg1 = walk_expr e1 in
    let ins2, ~result:reg2 = walk_expr e2 in
    let dest = Register.create () in
    let add_ins : Instruction.t = Add { dest; src1 = reg1; src2 = reg2 } in
    ins1 @ ins2 @ [ add_ins ], ~result:dest
  | _ -> failwith "todo"
;;
