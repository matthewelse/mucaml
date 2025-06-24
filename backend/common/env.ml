open! Core

let runtime_dir = "MUCAML_RUNTIME_LIB_DIR"

type t = { runtime_lib_dir : string } [@@deriving fields ~getters]

let create ?(runtime_lib_dir = ".") () =
  let runtime_lib_dir =
    match Sys.getenv runtime_dir with
    | Some dir -> dir
    | None -> runtime_lib_dir
  in
  { runtime_lib_dir }
;;

let runtime_lib_dir t triple =
  let mode = "release" in
  if Triple.is_host triple
  then t.runtime_lib_dir ^/ mode
  else t.runtime_lib_dir ^/ Triple.to_string triple ^/ mode
;;
