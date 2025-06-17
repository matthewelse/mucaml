open! Core

module T = struct
  type t = Cortex_M33 [@@deriving sexp_of, compare, enumerate, equal]

  let to_string = function
    | Cortex_M33 -> "cortex-m33"
  ;;

  let of_string = function
    | "cortex-m33" -> Cortex_M33
    | _ -> failwith "Unknown target architecture"
  ;;
end

include T

let arg_type = Command.Arg_type.enumerated (module T)
