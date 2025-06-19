open! Core

type t =
  { name : String_id.t
  ; top_level : string
  ; cpu : Target.t
  }

let parse_file filename =
  let open Result.Let_syntax in
  let%bind toml = Otoml.Parser.from_file_result filename in
  let%bind name =
    Otoml.find_result toml Otoml.get_string [ "name" ]
    |> Result.map ~f:String_id.of_string
  in
  let top_level =
    Otoml.find_or toml Otoml.get_string [ "top_level" ] ~default:"main.ml"
  in
  let%bind cpu =
    Otoml.find_result
      toml
      (fun toml -> Otoml.get_string toml |> Target.of_string)
      [ "target"; "cpu" ]
  in
  Ok { name; top_level; cpu }
;;
