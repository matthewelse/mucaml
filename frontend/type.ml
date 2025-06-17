open! Core

type t =
  | Int32
  | Bool
  | Unit
  | Fun of t * t
[@@deriving sexp_of]

let rec to_string = function
  | Int32 -> "int32"
  | Bool -> "bool"
  | Unit -> "unit"
  | Fun (t1, t2) -> Printf.sprintf "%s -> %s" (to_string t1) (to_string t2)
;;
