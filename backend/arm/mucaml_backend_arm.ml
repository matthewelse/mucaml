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
          (optional_with_default Cpu.Cortex_m33 Cpu.arg_type)
          ~doc:"CPU the target CPU architecture (default: Cortex_m33)"
      in
      { cpu }]
  ;;
end

let name = "arm"

let build_target_isa (triple : Triple.t) () =
  let open Or_error.Let_syntax in
  let%bind _use_hard_float =
    match triple with
    | { architecture = Arm V8m_main
      ; vendor = Unknown
      ; operating_system = None
      ; environment
      ; binary_format = Elf
      } ->
      let%bind use_hard_float =
        match environment with
        | Eabihf -> Ok true
        | Eabi -> Ok false
        | Gnu -> Or_error.error_string "GNU environment is not supported for ARM backend"
      in
      Ok use_hard_float
    | _ -> Or_error.error_string "Unsupported architecture for ARM backend"
  in
  Ok
    (module struct
      module Settings = Settings
      module Assembly = String

      let name = name
      let triple = triple
      let build_program cmm = Ok (Emit.emit_cmm cmm)
    end : Backend_intf.Target_isa)
;;
