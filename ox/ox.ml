include Core
module I32 = I32
module I64 = I64

type i32 = I32.t [@@deriving sexp_of]
type i64 = I64.t [@@deriving sexp_of]
