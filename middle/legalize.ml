open! Core
open! Import

(* Transform i64 operations into register pairs for targets that don't support native i64 *)

module Config = struct
  type t = { supports_native_i64 : bool }
end

(* Register pair mapping for i64 registers *)
module Register_pair_map = struct
  type t = (Mirl.Virtual_register.t, Mirl.Virtual_register.t * Mirl.Virtual_register.t) Hashtbl.t

  let create () = Hashtbl.create (module Mirl.Virtual_register)
  
  let get_or_create map func_builder orig_reg =
    match Hashtbl.find map orig_reg with
    | Some pair -> pair
    | None ->
      (* Create a new register pair for this i64 register *)
      let low_reg = Mirl.Function.Builder.fresh_register func_builder ~ty:Mirl.Type.I32 in
      let high_reg = Mirl.Function.Builder.fresh_register func_builder ~ty:Mirl.Type.I32 in
      Hashtbl.set map ~key:orig_reg ~data:(low_reg, high_reg);
      (low_reg, high_reg)
end

(* For now, simplify and just pass through all instructions.
   TODO: Implement proper i64 transformation using register type information *)
let legalize_instruction _pair_map _func_builder instruction =
  [ instruction ]

(* For now, just pass through functions unchanged.
   TODO: Implement proper function transformation *)
let legalize_function config func =
  ignore config;
  func

let legalize_program config program =
  if config.Config.supports_native_i64 then program
  else
    let open Mirl in
    let legalized_functions = List.map program.functions ~f:(legalize_function config) in
    { program with functions = legalized_functions }
;;