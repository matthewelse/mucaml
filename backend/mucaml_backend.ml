open! Core
open Mucaml_backend_common

let of_triple (triple : Triple.t) =
  match triple with
  | { architecture = Arm _; _ } -> Mucaml_backend_arm.build_target_isa triple ()
  | _ -> Or_error.error_string "Unsupported architecture for ARM backend"
;;
