open! Core
include Stdlib_upstream_compatible.Int32_u

let sexp_of_t t = Int32.sexp_of_t (to_int32 t)
let of_int_exn n = of_int32 (Int32.of_int_exn n)
let to_int_trunc n = Int32.to_int_trunc (to_int32 n)
