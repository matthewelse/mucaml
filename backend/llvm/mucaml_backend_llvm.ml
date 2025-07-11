open! Core
open! Async
open! Import

module Settings = struct
  type t = unit [@@deriving sexp_of]

  let param =
    [%map_open.Command
      let () = return () in
      ()]
  ;;
end

module Capabilities = struct
  let supports_native_i64 = true
end

let name = "llvm"

module Assembly = struct
  type t = string

  let to_string t = t
end

let build_target_isa (triple : Triple.t) (_ : Settings.t) =
  match triple.architecture with
  | Arm64 ->
    let module M = struct
      module Settings = Settings
      module Assembly = Assembly
      module Capabilities = Capabilities

      let name = name
      let triple = triple

      let build_program (mirl : Mirl.t) =
        let open Result.Let_syntax in
        let%bind llvm_ir = Llvm_ir_gen.generate mirl ~triple in
        return llvm_ir
      ;;

      let compile_and_link assembly ~env ~linker_args ~output_binary =
        let open Deferred.Result.Let_syntax in
        (* Write LLVM IR to temporary file *)
        let ll_file = output_binary ^ ".ll" in
        let%bind () =
          Monitor.try_with (fun () -> Writer.save ll_file ~contents:assembly)
          |> Deferred.Result.map_error ~f:(fun exn : Grace.Diagnostic.t ->
            { severity = Error
            ; message =
                (fun fmt ->
                  Format.pp_print_string
                    fmt
                    [%string "Failed to write LLVM IR file: %{Exn.to_string exn}"])
            ; labels = []
            ; notes = []
            })
        in
        (* Compile with llc *)
        let obj_file = output_binary ^ ".o" in
        let%bind () =
          Process.run_expect_no_output
            ~prog:"llc"
            ~args:[ "-filetype=obj"; "-o"; obj_file; ll_file ]
            ()
          |> Deferred.Result.map_error ~f:(fun err : Grace.Diagnostic.t ->
            { severity = Error
            ; message =
                (fun fmt ->
                  Format.pp_print_string
                    fmt
                    [%string "llc compilation failed: %{Error.to_string_hum err}"])
            ; labels = []
            ; notes = []
            })
        in
        (* Link with system linker *)
        let%bind () =
          Process.run_expect_no_output
            ~prog:"cc"
            ~args:
              ([ "-o"; output_binary; obj_file ]
               @ [ Env.runtime_lib_dir env ~board:"native" ^/ "libmucaml_runtime.a" ]
               @ linker_args)
            ()
          |> Deferred.Result.map_error ~f:(fun err : Grace.Diagnostic.t ->
            { severity = Error
            ; message =
                (fun fmt ->
                  Format.pp_print_string
                    fmt
                    [%string "linking failed: %{Error.to_string_hum err}"])
            ; labels = []
            ; notes = []
            })
        in
        return ()
      ;;
    end
    in
    Ok (module M : Backend_intf.Target_isa)
  | _ ->
    Or_error.error_string
      [%string
        "LLVM backend only supports aarch64 targets, got \
         %{triple.architecture#Triple.Architecture}"]
;;
