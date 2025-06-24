open! Core
open Mucaml_backend_common
module Arm = Mucaml_backend_arm
module Arm64 = Mucaml_backend_arm64

(* This module is the main entry point to constructing a target-specific backend. It's
   inspired by the way in which Cranelift constructs target ISA-specific backends.

   The backend is broken down by architecture, which each may be specialized to individual
   CPUs/features/etc.

   Per-architecture specialization is done via target-specific settings. This module has
   a generic representation of all possible settings, which we parse out as necessary. *)

module Settings = struct
  type t = { cpu : string option } [@@deriving fields ~getters]

  let default = { cpu = None }

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

let create (triple : Triple.t) (settings : Settings.t) =
  let open Or_error.Let_syntax in
  let parse_opt parse f ~error =
    match Option.map ~f:parse f with
    | exception exn ->
      Error (Error.of_exn exn |> Error.tag ~tag:(error (Option.value_exn f)))
    | s -> Ok s
  in
  let parse_req parse f ~error_if_invalid ~error_if_missing =
    match parse_opt parse f ~error:error_if_invalid with
    | Ok (Some cpu) -> Ok cpu
    | Ok None -> Or_error.error_string error_if_missing
    | Error _ as e -> e
  in
  (* TODO melse: Arguably this parsing is happening "too deep" in the program, and should
     be pushed up closer to command-line parsing. That will allow us to give clearer error
     messages about specifying the CPU in the TOML file, vs. via the command line. *)
  match triple.architecture with
  | Arm _ ->
    let%tydi { cpu } = settings in
    let%bind cpu =
      parse_req
        Arm.Cpu.of_string
        cpu
        ~error_if_missing:
          "No CPU specified for 32-bit ARM target. You should set the 'target.cpu' field \
           in the project file."
        ~error_if_invalid:(fun cpu -> [%string "Unknown CPU '%{cpu}'"])
    in
    Arm.build_target_isa triple { cpu }
  | Arm64 ->
    let%tydi { cpu } = settings in
    let%bind cpu =
      parse_opt Arm64.Cpu.of_string cpu ~error:(fun cpu ->
        [%string "Unknown CPU '%{cpu}'"])
    in
    Arm64.build_target_isa triple { cpu = Option.value cpu ~default:Arm64.Cpu.default }
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
    and params = Settings.param in
    ok_exn @@ create triple params]
;;
