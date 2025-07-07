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
  | Fun of t * t
[@@deriving sexp_of]

let rec to_string = function
  | Base ty -> Base.to_string ty
  | Fun (t1, t2) -> [%string "%{to_string t1} -> %{to_string t2}"]
;;
