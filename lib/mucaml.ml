open! Core
open! Async
open! Import
module E = MenhirLib.ErrorReports
module Ast = Ast
module Mirl = Mirl
module Parse = Parse
module Legalize = Mucaml_middle.Legalize

module Stage = struct
  module T = struct
    type t =
      | Ast
      | Mirl
      | Assembly
      | Compile
      | Run
    [@@deriving sexp_of, compare, enumerate, equal]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable ~case_sensitive:false (module T)
end

let run (project : Project.t) elf_file =
  let open Deferred.Or_error.Let_syntax in
  let command, args =
    match project.run_command with
    | None | Some [] -> elf_file, []
    | Some (cmd :: args) ->
      let subst s =
        match s with
        | "{elf_file}" -> elf_file
        | _ -> s
      in
      subst cmd, List.map args ~f:subst
  in
  let%bind () = Process.run_forwarding ~prog:command ~args () in
  return ()
;;

let compile_toplevel
  (module Target : Mucaml_backend_common.Backend_intf.Target_isa)
  input
  ~output_binary
  ~dump_stage
  ~linker_args
  ~env
  =
  let open Deferred.Or_error.Let_syntax in
  let files = Grace.Files.create () in
  match Parse.parse_toplevel input ~filename:"<stdin>" ~files with
  | Ok ast ->
    if [%compare.equal: Stage.t option] dump_stage (Some Ast)
    then Ast.to_string_hum ast |> print_endline;
    let cmm = Mirl.of_ast ast in
    if [%compare.equal: Stage.t option] dump_stage (Some Mirl)
    then Mirl.to_string cmm |> print_endline;
    let legalize_config = Legalize.Config.{ supports_native_i64 = Target.Capabilities.supports_native_i64 } in
    let legalized_cmm = Legalize.legalize_program legalize_config cmm in
    let%bind assembly = Deferred.return (Target.build_program legalized_cmm) in
    (*
       let rpi_build_info =
      Rpi_binary_info.generate_assembly
        [ T
            { type_ = IdAndString
            ; namespace = 'R', 'P'
            ; value = Rpi_binary_info.Entry.Id.rp_program_name, program_name
            }
        ; T
            { type_ = IdAndString
            ; namespace = 'R', 'P'
            ; value = Rpi_binary_info.Entry.Id.rp_pico_board, "adafruit_feather_rp2350"
            }
        ; T
            { type_ = IdAndIntLabel
            ; namespace = 'R', 'P'
            ; value = Rpi_binary_info.Entry.Id.rp_binary_end, "__flash_binary_end"
            }
        ]
    in
    *)
    if [%compare.equal: Stage.t option] dump_stage (Some Assembly)
    then (
      let assembly = Target.Assembly.to_string assembly in
      print_endline assembly);
    let%bind () = Target.compile_and_link assembly ~env ~linker_args ~output_binary in
    return ()
  | Error diagnostic ->
    let diagnostic_config = Grace_rendering.Config.default in
    Fmt_doc.render
      Fmt.stderr
      Grace_rendering.(ppd_rich ~config:diagnostic_config ~files diagnostic);
    Format.fprintf Fmt.stderr "%!";
    return ()
;;

let repl =
  Command.async_or_error
    ~summary:"mucaml repl"
    [%map_open.Command
      let dump_stage =
        flag "dump-stage" (optional Stage.arg_type) ~doc:"STAGE the stage to print out"
      and (module Backend) = Mucaml_backend.target_param
      and should_run =
        flag "run" no_arg ~doc:"RUN whether to run the compiled program after compilation"
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let env = Env.create () in
        let project = Project.native_repl in
        let%bind () =
          Deferred.Or_error.repeat_until_finished () (fun () ->
            match LNoise.linenoise "> " with
            | None -> return (`Finished ())
            | Some input ->
              (* The only time [history_add] returns an error is when you have a
                 duplicate message, which is not a problem. *)
              LNoise.history_add input |> (ignore : (unit, string) Result.t -> unit);
              let output_binary = "program.elf" in
              let%bind () =
                compile_toplevel
                  (module Backend)
                  input
                  ~output_binary
                  ~dump_stage
                  ~linker_args:[]
                  ~env
              in
              let%bind () = if should_run then run project output_binary else return () in
              return (`Repeat ()))
        in
        return ()]
;;

let async_or_diagnostic ~summary param =
  let files = Grace.Files.create () in
  Command.async
    ~summary
    [%map_open.Command
      let param = param in
      fun () ->
        match%bind param ~files () with
        | Ok () -> return ()
        | Error diagnostic ->
          let diagnostic_config = Grace_rendering.Config.default in
          Fmt_doc.render
            Fmt.stderr
            Grace_rendering.(ppd_rich ~config:diagnostic_config ~files diagnostic);
          Format.fprintf Fmt.stderr "%!";
          Shutdown.shutdown 1;
          return ()]
;;

let build ~should_run =
  async_or_diagnostic
    ~summary:"mucaml build tool"
    [%map_open.Command
      let project_file =
        flag
          "project"
          (optional_with_default "mucaml.toml" string)
          ~doc:"FILE path to project file (default: mucaml.toml)"
      and dump_stage =
        flag "dump-stage" (optional Stage.arg_type) ~doc:"STAGE the stage to print out"
      in
      fun ~files () ->
        let open Deferred.Result.Let_syntax in
        let env = Env.create () in
        let wrap =
          let open Grace in
          Result.map_error ~f:(fun error : Diagnostic.t ->
            { severity = Error
            ; message =
                (fun fmt -> Format.fprintf fmt "Error: %s" (Error.to_string_hum error))
            ; labels = []
            ; notes = []
            })
        in
        let%bind project = Project.parse_file project_file ~files |> Deferred.return in
        let%bind (module Target) =
          Deferred.return
            (wrap (Mucaml_backend.create project.target project.backend_params))
        in
        let%bind code =
          Deferred.map ~f:wrap
          @@ Deferred.Or_error.try_with (fun () -> Reader.file_contents project.top_level)
        in
        let output_binary = [%string "%{project.name#String_id}.elf"] in
        let%bind () =
          Deferred.map ~f:wrap
          @@ compile_toplevel
               (module Target)
               code
               ~dump_stage
               ~output_binary
               ~linker_args:project.linker_args
               ~env
        in
        let%bind () =
          Deferred.map ~f:wrap
          @@ if should_run then run project output_binary else return ()
        in
        return ()]
;;

let main () =
  Command_unix.run
  @@ Command.group
       ~summary:"mucaml"
       [ "repl", repl; "build", build ~should_run:false; "run", build ~should_run:true ]
;;
