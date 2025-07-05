open! Core
open! Import
module Cpu = Cpu

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

module Capabilities = struct
  let supports_native_i64 = false
end

let name = "arm"

let build_target_isa (triple : Triple.t) ({ cpu } : Settings.t) =
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
      module Capabilities = Capabilities

      let name = name
      let triple = triple
      let build_program mirl = Ok (Emit.emit_mirl mirl)

      let compile_and_link program ~env ~linker_args ~output_binary =
        let open Async in
        let open Deferred.Or_error.Let_syntax in
        let link_command = "arm-none-eabi-gcc" in
        let args =
          linker_args
          @ [ "-L" ^ Env.runtime_lib_dir env; "-lmucaml_runtime" ]
          @ [ "-g"
            ; "-mcpu=" ^ Cpu.to_string cpu
            ; "-nostdlib"
            ; "-x"
            ; "assembler"
            ; "-"
            ; "-o"
            ; output_binary
            ]
        in
        let%bind () =
          Process.run_expect_no_output
            ~prog:link_command
            ~args
            ~stdin:(Assembly.to_string program)
            ()
        in
        return ()
      ;;
    end : Backend_intf.Target_isa)
;;
