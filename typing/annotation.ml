open! Ox
open! Import

type t =
  | Expected_type of (Ast.Expr.t * Type.t * but:Type.t option * defined:Location.t option)
[@@deriving sexp_of]
