open! Core

let parse text =
  let files = Grace.Files.create () in
  match Mucaml.Parse.parse_toplevel text ~filename:"<test>" ~files with
  | Ok ast -> Ok ast
  | Error diagnostic ->
    Fmt_doc.render
      Fmt.stderr
      Grace_rendering.(
        ppd_rich
          ~config:{ Grace_rendering.Config.default with color = false }
          ~files
          diagnostic);
    Format.fprintf Fmt.stderr "%!";
    Error ()
;;
