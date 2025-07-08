open! Ox
open! Import

type t = Expression_should_have_type of Ast.Expr.t * Type.t [@@deriving sexp_of]
