open! Core
open! Async
open! Import
module E = MenhirLib.ErrorReports
module Ast = Ast
module Cmm = Cmm
module Parse = Parse

module Stage = struct
  module T = struct
    type t =
      | Ast
      | Cmm
      | Assembly
      | Compile
      | Run
    [@@deriving sexp_of, compare, enumerate, equal]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable ~case_sensitive:false (module T)
end

let compile_and_link assembly =
  Out_channel.write_all "runtime/src/program.s" ~data:assembly;
  Process.run_expect_no_output ~prog:"cargo" ~working_dir:"runtime" ~args:[ "build" ] ()
;;

let run_toplevel (module Target : Backend_intf.S) input ~dump_stage =
  let open Deferred.Or_error.Let_syntax in
  let files = Grace.Files.create () in
  (* The only time [history_add] returns an error is when you have a duplicate  message,
     which is not a problem. *)
  LNoise.history_add input |> (ignore : (unit, string) Result.t -> unit);
  match Parse.parse_toplevel input ~filename:"<stdin>" ~files with
  | Ok ast ->
    if Stage.equal dump_stage Ast then Ast.pprint_prog ast;
    let cmm = Cmm.of_ast ast in
    if Stage.equal dump_stage Cmm then Cmm.to_string cmm |> print_endline;
    let assembly = Target.Emit.emit_cmm cmm in
    if Stage.equal dump_stage Assembly then print_endline assembly;
    let%bind () = compile_and_link assembly in
    let%bind () =
      Process.run_forwarding ~prog:"cargo" ~working_dir:"runtime" ~args:[ "run"; "-q" ] ()
    in
    return ()
  | Error diagnostic ->
    let diagnostic_config = Grace_rendering.Config.default in
    Fmt_doc.render
      Fmt.stderr
      Grace_rendering.(ppd_rich ~config:diagnostic_config ~files diagnostic);
    Format.fprintf Fmt.stderr "%!";
    return ()
;;

let command =
  Command.async_or_error
    ~summary:"mucaml frontend demo"
    [%map_open.Command
      let dump_stage =
        flag "dump-stage" (required Stage.arg_type) ~doc:"STAGE the stage to print out"
      and target =
        flag
          "target"
          (optional_with_default Target.Cortex_M33 Target.arg_type)
          ~doc:"TARGET the target architecture to emit assembly for"
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let (module Target) =
          match target with
          | Cortex_M33 -> (module Mucaml_backend_arm : Backend_intf.S)
        in
        let%bind () =
          Deferred.Or_error.repeat_until_finished () (fun () ->
            match LNoise.linenoise "> " with
            | None -> return (`Finished ())
            | Some input ->
              let%bind () = run_toplevel (module Target) input ~dump_stage in
              return (`Repeat ()))
        in
        return ()]
;;

let main () = Command_unix.run command
