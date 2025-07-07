open! Core

(** [parse_toplevel input] parses a top-level expression from the given [input] string.
    Returns [Ok prog] on success, where [prog] is the parsed program, or [Error ()] on
    failure. *)
val parse_toplevel
  :  string
  -> file_id:Grace.File.Id.t
  -> (Ast.t, Grace.Diagnostic.t) result
