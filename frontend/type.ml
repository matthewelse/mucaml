open! Core

module Base = struct
  type t =
    | I32
    | I64
    | Bool
    | Unit
  [@@deriving string ~capitalize:"snake_case", sexp_of]
end

type t =
  | Base of Base.t
  | Fun of t list * t
[@@deriving sexp_of]

let rec to_string = function
  | Base ty -> Base.to_string ty
  | Fun ([ arg_type ], ret_type) ->
    let arg_str = to_string arg_type in
    [%string "%{arg_str} -> %{to_string ret_type}"]
  | Fun (arg_types, ret_type) ->
    let args_str = String.concat ~sep:"," (List.map arg_types ~f:to_string) in
    [%string "(%{args_str}) -> %{to_string ret_type}"]
;;

