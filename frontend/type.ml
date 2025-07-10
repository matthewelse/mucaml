open! Core

module Base = struct
  type t =
    | I32
    | I64
    | Bool
    | Unit
  [@@deriving compare, equal, string ~capitalize:"snake_case", sexp_of]
end

module Type_class = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let to_string t = t
end

module Constraint = struct
  type t =
    { var : string
    ; type_class : Type_class.t
    }
  [@@deriving sexp_of]

  let to_string { var; type_class } = [%string "%{var} : %{type_class#Type_class}"]
end

type t =
  | Base of Base.t
  | Fun of t list * t
  | Var of string
  | Constrained of Constraint.t list * t
[@@deriving sexp_of]

let rec to_string = function
  | Base ty -> Base.to_string ty
  | Fun ([ arg_type ], ret_type) ->
    let arg_str = to_string arg_type in
    [%string "%{arg_str} -> %{to_string ret_type}"]
  | Fun (arg_types, ret_type) ->
    let args_str = String.concat ~sep:"," (List.map arg_types ~f:to_string) in
    [%string "(%{args_str}) -> %{to_string ret_type}"]
  | Var var -> [%string "'%{var}"]
  | Constrained (constraints, ty) ->
    let constraints_str =
      String.concat ~sep:", " (List.map constraints ~f:Constraint.to_string)
    in
    [%string "(%{constraints_str}). %{to_string ty}"]
;;
