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

      let compile_and_link program ~env:(_ : Env.t) ~linker_args ~output_binary =
        let open Async in
        let open Deferred.Or_error.Let_syntax in
        let link_command = "gcc" in
        (* TODO: Link a real runtime. For the time being, just call directly into the
           main function. *)
        let args =
          linker_args
          @ [ "-g"
            ; "-mcpu=" ^ Cpu.to_string cpu
            ; "-x"
            ; "assembler"
            ; "-"
            ; "-o"
            ; output_binary
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

.type mucaml_print, %function
.globl mucaml_print
mucaml_print:
  stp     x29, x30, [sp, #-32]!
  str     x19, [sp, #16]
  mov     x29, sp
  mov     w19, w0
  adrp    x0, mucaml_print.str
  add     x0, x0, :lo12:mucaml_print.str
  mov     w1, w19
  bl      printf
  mov     w0, w19
  ldr     x19, [sp, #16]
  ldp     x29, x30, [sp], #32
  ret

mucaml_print.str:
  .asciz "%d\n"

%{program#Assembly}|}]
        in
        let%bind () =
          Process.run_expect_no_output ~prog:link_command ~args ~stdin:assembly ()
        in
        return ()
      ;;
    end : Backend_intf.Target_isa)
;;
