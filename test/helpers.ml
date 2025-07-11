open! Core
open! Import

let render_error diagnostic ~files =
  Fmt_doc.render
    Fmt.stderr
    Grace_rendering.(
      ppd_rich ~config:{ Config.default with color = false } ~files diagnostic);
  Format.fprintf Fmt.stderr "\n%!"
;;

let with_file text f =
  let files = Grace.Files.create () in
  let file_id = Grace.Files.add files "<test>" text in
  match f file_id with
  | Ok x -> Some x
  | Error diagnostic ->
    render_error diagnostic ~files;
    None
;;

let parse' text ~file_id = Mucaml.Parse.parse_toplevel text ~file_id
let parse text = with_file text (fun file_id -> parse' text ~file_id)

let typecheck text =
  with_file text (fun file_id ->
    let%bind.Result ast = parse' text ~file_id in
    Mucaml_typing.type_ast ast ~file_id)
;;

let compile ?(target = "thumbv8m.main-none-eabi") ?(board = Some "rp2350") text =
  let (module Target) =
    Mucaml_backend.create
      (Mucaml_backend_common.Triple.of_string target)
      { board; backend = None }
    |> ok_exn
  in
  match
    with_file text (fun file_id ->
      let%bind.Result ast = parse' text ~file_id in
      let%bind.Result ast = Mucaml_typing.type_ast ast ~file_id in
      let mirl = Mirl.of_ast ast in
      Target.build_program mirl)
  with
  | Some assembly -> print_endline (Target.Assembly.to_string assembly)
  | None -> ()
;;
