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

let runtime_lib_dir t ~board = t.runtime_lib_dir ^/ board
