open! Core
include Stdlib_upstream_compatible.Int64_u

let sexp_of_t t = Int64.sexp_of_t (to_int64 t)
let to_int_trunc n = Int64.to_int_trunc (to_int64 n)
