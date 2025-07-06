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
      | Legalized
      | Assembly
      | Compile
    [@@deriving sexp_of, compare, enumerate, equal]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable ~case_sensitive:false (module T)

  module type S = sig
    module Input : T

    module Output : sig
      type t

      val to_string_hum : t -> string
    end

    val name : t
    val run : Input.t -> Output.t Deferred.Or_error.t
  end

  module Pipeline = struct
    type stage = t [@@deriving equal]

    type ('input, 'output) t =
      | [] : ('input, 'input) t
      | ( :: ) :
          (module S with type Input.t = 'input and type Output.t = 'inter)
          * ('inter, 'output) t
          -> ('input, 'output) t

    let rec run
      : type input output.
        (input, output) t
        -> input
        -> stop_after:stage option
        -> dump_stage:stage option
        -> output option Deferred.Or_error.t
      =
      fun t input ~stop_after ~dump_stage ->
      match t with
      | [] -> Deferred.Or_error.return (Some input)
      | (module Stage) :: tl ->
        let%bind.Deferred.Or_error output = Stage.run input in
        if [%equal: stage option] (Some Stage.name) dump_stage
        then print_endline (Stage.Output.to_string_hum output);
        if [%equal: stage option] (Some Stage.name) stop_after
        then Deferred.Or_error.return None
        else run tl output ~stop_after ~dump_stage
    ;;

    let run' t i ~stop_after ~dump_stage =
      let open Deferred.Or_error.Let_syntax in
      let%bind (None | Some ()) = run t i ~stop_after ~dump_stage in
      return ()
    ;;
  end
end

module How_to_run = struct
  type _ t =
    | Subprocess : unit Or_error.t Deferred.t t
    | Replace_current_process : never_returns t
end

let run (type r) (project : Project.t) elf_file ~(how : r How_to_run.t) : r =
  let open Deferred.Or_error.Let_syntax in
  let elf_file = "./" ^ elf_file in
  let command, args =
    match project.run_command with
    | None | Some [] -> elf_file, []
    | Some (cmd :: args) ->
      let subst s =
        let pat = String.Search_pattern.create "{elf_file}" in
        String.Search_pattern.replace_all pat ~in_:s ~with_:elf_file
      in
      subst cmd, List.map args ~f:subst
  in
  match how with
  | Subprocess ->
    let%bind () = Process.run_forwarding ~prog:command ~args () in
    return ()
  | Replace_current_process ->
    Nothing.unreachable_code @@ Core_unix.exec ~prog:command ~argv:(command :: args) ()
;;

let stages
  (module Target : Mucaml_backend_common.Backend_intf.Target_isa)
  ~env
  ~linker_args
  ~output_binary
  : (Ast.t, _) Stage.Pipeline.t
  =
  let stage (type i o) ~(output_to_string_hum : o -> string) ~run name
    : (module Stage.S with type Input.t = i and type Output.t = o)
    =
    (module struct
      module Input = struct
        type t = i
      end

      module Output = struct
        type t = o

        let to_string_hum = output_to_string_hum
      end

      let name = name
      let run = run
    end)
  in
  [ stage
      ~output_to_string_hum:Ast.to_string_hum
      ~run:(fun x -> Deferred.Or_error.return x)
      Ast
  ; stage
      ~output_to_string_hum:Mirl.to_string
      ~run:(fun x -> Deferred.Or_error.return (Mirl.of_ast x))
      Mirl
  ; stage
      ~output_to_string_hum:Mirl.to_string
      ~run:(fun x ->
        Deferred.Or_error.return
          (Legalize.legalize_program
             { supports_native_i64 = Target.Capabilities.supports_native_i64 }
             x))
      Legalized
  ; stage
      ~output_to_string_hum:Target.Assembly.to_string
      ~run:(fun mirl -> Deferred.return @@ Target.build_program mirl)
      Assembly
  ; stage
      ~output_to_string_hum:Unit.to_string
      ~run:(fun assembly ->
        Target.compile_and_link assembly ~env ~linker_args ~output_binary)
      Compile
  ]
;;

let compile_toplevel
  ?(stop_after : Stage.t option)
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
    Stage.Pipeline.run'
      (stages (module Target) ~env ~linker_args ~output_binary)
      ast
      ~stop_after
      ~dump_stage
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
      and (module Backend) = Mucaml_backend.target_param in
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
              let%bind () = run project output_binary ~how:Subprocess in
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
      and stop_after =
        flag
          "stop-after"
          (optional Stage.arg_type)
          ~doc:"STAGE stop compiling after this stage"
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
               ?stop_after
               (module Target)
               code
               ~dump_stage
               ~output_binary
               ~linker_args:project.linker_args
               ~env
        in
        let%bind () =
          Deferred.map ~f:wrap
          @@
          if should_run
          then
            Nothing.unreachable_code
            @@ run project output_binary ~how:Replace_current_process
          else return ()
        in
        return ()]
;;

let main () =
  Command_unix.run
  @@ Command.group
       ~summary:"mucaml"
       [ "repl", repl; "build", build ~should_run:false; "run", build ~should_run:true ]
;;
