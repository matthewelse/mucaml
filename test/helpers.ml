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
  let target = "thumbv8m.main-none-eabi" in
  let (module Target) =
    Mucaml_backend.of_triple (Mucaml_backend_common.Triple.of_string target) |> ok_exn
  in
  match parse text with
  | Ok ast ->
    let cmm = Cmm.of_ast ast in
    let assembly = Target.build_program cmm |> ok_exn in
    print_endline (Target.Assembly.to_string assembly)
  | Error () -> ()
;;
