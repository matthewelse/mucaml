open! Core
open Mucaml_backend_common

let of_triple (triple : Triple.t) =
  match triple with
  | { architecture = Arm _; _ } -> Mucaml_backend_arm.build_target_isa triple ()
  | { architecture = Arm64; _ } -> Mucaml_backend_arm64.build_target_isa triple ()
;;
