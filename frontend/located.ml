open! Ox
open! Import

type 'a t =
  { txt : 'a @@ global
  ; loc : Location.t
  }
[@@deriving sexp_of]
