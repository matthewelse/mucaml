open! Core

type t =
  | Int32
  | Bool
  | Unit
  | Fun of t * t
[@@deriving sexp_of]

let rec string_of_type = function
  | Int32 -> "int32"
  | Bool -> "bool"
  | Unit -> "unit"
  | Fun (t1, t2) -> Printf.sprintf "%s -> %s" (string_of_type t1) (string_of_type t2)
;;
