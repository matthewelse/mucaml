open! Core
open! Import

let render_error diagnostic ~files =
  Fmt_doc.render
    Fmt.stderr
    Grace_rendering.(
      ppd_rich ~config:{ Config.default with color = false } ~files diagnostic);
  Format.fprintf Fmt.stderr "\n%!"
;;

let parse' text ~file_id = Mucaml.Parse.parse_toplevel text ~file_id

let parse text =
  let files = Grace.Files.create () in
  let file_id = Grace.Files.add files "<test>" text in
  match parse' text ~file_id with
  | Ok ast -> Ok ast
  | Error diagnostic ->
    render_error diagnostic ~files;
    Error ()
;;

let typecheck text =
  let files = Grace.Files.create () in
  let file_id = Grace.Files.add files "<test>" text in
  match
    let%bind.Result ast = parse' text ~file_id in
    Mucaml_typing.type_ast ast ~file_id
  with
  | Ok env -> Some env
  | Error diagnostic ->
    render_error diagnostic ~files;
    None
;;

let compile text =
  let target = "thumbv8m.main-none-eabi" in
  let (module Target) =
    Mucaml_backend.create
      (Mucaml_backend_common.Triple.of_string target)
      { board = Some "rp2350" }
    |> ok_exn
  in
  match parse text with
  | Ok ast ->
    let mirl = Mirl.of_ast ast in
    let assembly = Target.build_program mirl |> ok_exn in
    print_endline (Target.Assembly.to_string assembly)
  | Error () -> ()
;;
