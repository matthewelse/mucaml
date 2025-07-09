open! Ox
open! Import

type t = Same_type of Type.t * Type.t * (Annotation.t Nonempty_list.t[@sexp.opaque])
[@@deriving sexp_of]
