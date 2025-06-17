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
  [@@deriving sexp_of]

  let to_string = function
    | Add { dest; src1; src2 } ->
      [%string "%{dest#Register} := %{src1#Register} + %{src2#Register}"]
    | Sub { dest; src1; src2 } ->
      [%string "%{dest#Register} := %{src1#Register} - %{src2#Register}"]
    | Set { dest; value } -> [%string "%{dest#Register} := %{value#Int}"]
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
    { instructions : Instruction.t Doubly_linked.t
    ; terminator : Terminator.t
    }
  [@@deriving sexp_of]

  let to_string ?(indent = "") { instructions; terminator } =
    String.concat
      ~sep:"\n"
      (List.map (Doubly_linked.to_list instructions) ~f:(fun instruction ->
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
    let acc = Doubly_linked.create () in
    let result = walk_expr body ~acc in
    let block = { Block.instructions = acc; terminator = Return result } in
    let function_ =
      { Function.name = [%string "mucaml_%{name}"]; params; body = block }
    in
    { functions = [ function_ ] }

and walk_expr (expr : Ast.expr) ~(acc : Instruction.t Doubly_linked.t) : Register.t =
  match expr with
  | Int i ->
    let reg = Register.create () in
    Doubly_linked.insert_last acc (Set { dest = reg; value = i })
    |> (ignore : _ Doubly_linked.Elt.t -> unit);
    reg
  | App (Var "+", [ e1; e2 ]) ->
    let reg1 = walk_expr e1 ~acc in
    let reg2 = walk_expr e2 ~acc in
    let dest = Register.create () in
    let add_ins : Instruction.t = Add { dest; src1 = reg1; src2 = reg2 } in
    Doubly_linked.insert_last acc add_ins |> (ignore : _ Doubly_linked.Elt.t -> unit);
    dest
  | App (Var "-", [ e1; e2 ]) ->
    let reg1 = walk_expr e1 ~acc in
    let reg2 = walk_expr e2 ~acc in
    let dest = Register.create () in
    let sub_ins : Instruction.t = Sub { dest; src1 = reg1; src2 = reg2 } in
    Doubly_linked.insert_last acc sub_ins |> (ignore : _ Doubly_linked.Elt.t -> unit);
    dest
  | _ -> failwith "todo"
;;
