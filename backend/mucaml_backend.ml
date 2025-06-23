open! Core
open Mucaml_backend_common
module Arm = Mucaml_backend_arm
module Arm64 = Mucaml_backend_arm64

module Generic_params = struct
  type t = { cpu : string option } [@@deriving fields ~getters]

  let param =
    [%map_open.Command
      let cpu =
        flag
          "cpu"
          ~aliases:[ "mcpu" ]
          (optional string)
          ~doc:"CPU the target CPU architecture"
      in
      { cpu }]
  ;;

  let of_toml (toml : Otoml.t) =
    let cpu = Otoml.find_opt toml Otoml.get_string [ "target"; "cpu" ] in
    { cpu }
  ;;
end

let create (triple : Triple.t) ({ cpu } : Generic_params.t) =
  match triple.architecture with
  | Arm _ ->
    Arm.build_target_isa
      triple
      { cpu =
          Option.map cpu ~f:Arm.Cpu.of_string
          |> Option.value_exn ~message:"Missing ARM CPU string"
      }
  | Arm64 ->
    Arm64.build_target_isa
      triple
      { cpu = Option.value_map cpu ~f:Arm64.Cpu.of_string ~default:Arm64.Cpu.default }
;;

let target_param =
  [%map_open.Command
    let triple =
      flag
        "target"
        (optional_with_default Triple.default Triple.arg_type)
        ~doc:
          [%string
            "TARGET target architecture to build for (default: %{Triple.default#Triple})"]
    and params = Generic_params.param in
    ok_exn @@ create triple params]
;;
