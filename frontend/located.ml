open! Ox
open! Import

type 'a t =
  { txt : 'a
  ; loc : Location.t
  }
[@@deriving sexp_of]
