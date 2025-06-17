open! Core

type t = Cortex_M33 [@@deriving sexp_of, compare, enumerate, equal, string]

val to_string : t -> string
val arg_type : t Command.Arg_type.t
