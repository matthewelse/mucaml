open! Core

(** [parse_toplevel input] parses a top-level expression from the given [input] string.
    Returns [Ok prog] on success, where [prog] is the parsed program, or [Error ()] on
    failure. *)
val parse_toplevel
  :  string
  -> filename:string
  -> files:Grace.Files.t
  -> (Ast.prog, Grace.Diagnostic.t) result
