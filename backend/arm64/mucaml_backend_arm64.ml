open! Core
open! Import

module Settings = struct
  type t = { cpu : Cpu.t } [@@deriving sexp_of]

  let param =
    [%map_open.Command
      let cpu =
        flag
          "cpu"
          ~aliases:[ "mcpu" ]
          (optional_with_default Cpu.Apple_m1 Cpu.arg_type)
          ~doc:"CPU the target CPU architecture"
      in
      { cpu }]
  ;;
end

let name = "arm64"

let build_target_isa (triple : Triple.t) () =
  let open Or_error.Let_syntax in
  let%bind () =
    match triple with
    | { architecture = Arm64
      ; vendor = Unknown
      ; operating_system = Linux
      ; environment = Gnu
      ; binary_format = Elf
      } -> Ok ()
    | _ -> Or_error.error_string "Unsupported architecture for ARM64 backend"
  in
  Ok
    (module struct
      module Settings = Settings
      module Assembly = String

      let name = name
      let triple = triple
      let build_program _ = Or_error.error_string "TODO"
    end : Backend_intf.Target_isa
      with type Settings.t = Settings.t)
;;
