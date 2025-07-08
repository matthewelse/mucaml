open! Ox
open! Import

type t = Same_type of Type.t * Type.t [@@deriving sexp_of]
