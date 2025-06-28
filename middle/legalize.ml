open! Core
open! Import

(* Transform i64 operations into register pairs for targets that don't support native i64 *)

module Config = struct
  type t = { supports_native_i64 : bool }
end

(* For now, we'll just pass through the program unchanged until we properly 
   implement the MIRL transformation with the builder interface *)
let legalize_program config program =
  ignore config;
  program
;;