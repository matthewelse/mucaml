open! Core

module T = struct
  type t = Cortex_m33
  [@@deriving sexp_of, compare, enumerate, equal, string ~capitalize:"kebab-case"]
end

include T

let arg_type =
  Command.Arg_type.enumerated ~case_sensitive:false ~list_values_in_help:true (module T)
;;
