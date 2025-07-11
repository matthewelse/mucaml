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
          (optional_with_default Cpu.Apple_m1 Cpu.arg_type)
          ~doc:"CPU the target CPU architecture"
      in
      { cpu }]
  ;;
end

module Capabilities = struct
  let supports_native_i64 = true
end

let name = "arm64"

let build_target_isa (triple : Triple.t) ({ cpu } : Settings.t) =
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
      module Capabilities = Capabilities

      let name = name
      let triple = triple
      let build_program program = Ok (Emit.emit_mirl program)

      let compile_and_link program ~env ~linker_args ~output_binary =
        let open Async in
        let open Deferred.Result.Let_syntax in
        let asm_command = "gcc" in
        let object_name = Filename.basename output_binary ^ ".o" in
        let args =
          [ "-g"
          ; "-ffunction-sections"
          ; "-mcpu=" ^ Cpu.to_string cpu
          ; "-x"
          ; "assembler"
          ; "-"
          ; "-c"
          ; "-o"
          ; object_name
          ]
        in
        let assembly =
          [%string
            {|.type main, %function
.globl main
main:
mov w0, #0
b mucaml_main
.size main, . - main

%{program#Assembly}|}]
        in
        let%bind () =
          Process.run_expect_no_output ~prog:asm_command ~args ~stdin:assembly ()
          |> Deferred.Result.map_error ~f:(fun error : Grace.Diagnostic.t ->
            { severity = Error
            ; message =
                (fun fmt -> Format.pp_print_string fmt (Error.to_string_hum error))
            ; labels = []
            ; notes = []
            })
        in
        let link_command = "gcc" in
        let args =
          [ "-Wl,--gc-sections" ]
          @ linker_args
          @ [ (* It's important that [object_name] precedes the library, since it depends
                 on [libmucaml_runtime]. *)
              object_name
            ; Env.runtime_lib_dir env ~board:"native" ^/ "libmucaml_runtime.a"
            ]
          @ [ "-o"; output_binary ]
        in
        let%bind () =
          Process.run_expect_no_output
            ~prog:link_command
            ~args
            ~stdin:(Assembly.to_string program)
            ()
          |> Deferred.Result.map_error ~f:(fun error : Grace.Diagnostic.t ->
            { severity = Error
            ; message =
                (fun fmt -> Format.pp_print_string fmt (Error.to_string_hum error))
            ; labels = []
            ; notes = []
            })
        in
        return ()
      ;;
    end : Backend_intf.Target_isa)
;;
