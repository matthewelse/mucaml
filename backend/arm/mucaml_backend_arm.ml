open! Core
open! Import

module Settings = struct
  type t = unit [@@deriving sexp_of]

  let param = Command.Param.return ()
end

let name = "arm"

let build_target_isa (triple : Triple.t) () =
  let open Or_error.Let_syntax in
  let%bind _use_hard_float =
    match triple with
    | { architecture = Arm
      ; vendor = Unknown
      ; operating_system = None
      ; environment
      ; binary_format = Elf
      } ->
      let%bind use_hard_float =
        match environment with
        | Eabihf -> Ok true
        | Eabi -> Ok false
      in
      Ok use_hard_float
    | _ -> Or_error.error_string "Unsupported architecture for ARM backend"
  in
  Ok
    (module struct
      module Settings = Settings
      module Assembly = String

      let name = "arm"
      let triple = triple
      let build_program cmm = Ok (Emit.emit_cmm cmm)
    end : Backend_intf.Target_isa
      with type Settings.t = Settings.t)
;;
