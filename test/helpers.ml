open! Core
open! Import

let parse text =
  Cmm.Register.For_testing.reset_counter ();
  let files = Grace.Files.create () in
  match Mucaml.Parse.parse_toplevel text ~filename:"<test>" ~files with
  | Ok ast -> Ok ast
  | Error diagnostic ->
    Fmt_doc.render
      Fmt.stderr
      Grace_rendering.(
        ppd_rich ~config:{ Config.default with color = false } ~files diagnostic);
    Format.fprintf Fmt.stderr "%!";
    Error ()
;;

let compile text =
  match parse text with
  | Ok ast ->
    let cmm = Cmm.of_ast ast in
    let assembly = Emit.For_testing.emit_cmm_without_prologue cmm in
    print_endline assembly
  | Error () -> ()
;;
