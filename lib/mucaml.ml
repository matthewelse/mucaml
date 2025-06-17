open! Core
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
    [@@deriving sexp_of, compare, enumerate, equal]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable (module T)
end

let command =
  Command.basic
    ~summary:"mucaml frontend demo"
    [%map_open.Command
      let stage =
        flag "stage" (required Stage.arg_type) ~doc:"STAGE the stage to print out"
      and target =
        flag
          "target"
          (optional_with_default Target.Cortex_M33 Target.arg_type)
          ~doc:"TARGET the target architecture to emit assembly for"
      in
      fun () ->
        let (module Target) =
          match target with
          | Cortex_M33 -> (module Mucaml_backend_arm : Backend_intf.S)
        in
        while
          match LNoise.linenoise "> " with
          | None -> false
          | Some input ->
            let files = Grace.Files.create () in
            LNoise.history_add input |> Result.ok_or_failwith;
            (match Parse.parse_toplevel input ~filename:"<stdin>" ~files with
             | Ok ast ->
               if Stage.equal stage Ast
               then Ast.pprint_prog ast
               else (
                 let cmm = Cmm.of_ast ast in
                 if Stage.equal stage Cmm
                 then Cmm.to_string cmm |> print_endline
                 else if Stage.equal stage Assembly
                 then (
                   let assembly = Target.Emit.emit_cmm cmm in
                   print_endline assembly)
                 else ());
               true
             | Error diagnostic ->
               let diagnostic_config = Grace_rendering.Config.default in
               Fmt_doc.render
                 Fmt.stderr
                 Grace_rendering.(ppd_rich ~config:diagnostic_config ~files diagnostic);
               Format.fprintf Fmt.stderr "%!";
               false)
        do
          ()
        done]
;;

let main () = Command_unix.run command
