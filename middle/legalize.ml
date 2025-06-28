open! Core
open! Import

(* Transform i64 operations into register pairs for targets that don't support native i64 *)

module Config = struct
  type t = { supports_native_i64 : bool }
end

(* Register type lookup table from original function *)
module Register_types = struct
  type t = Mirl.Type.t array

  let create (func : Mirl.Function.t) =
    Iarray.to_array func.registers |> Array.map ~f:(fun { ty } -> ty)
  ;;

  let get_type types reg =
    let index = Mirl.Virtual_register.to_int_exn reg in
    if index < Array.length types then types.(index) else Mirl.Type.I32
  ;;

  let is_i64 types reg =
    match get_type types reg with
    | Mirl.Type.I64 -> true
    | Mirl.Type.I32 -> false
  ;;
end

(* Register pair mapping for i64 registers *)
module Register_pair_map = struct
  type t =
    (Mirl.Virtual_register.t, Mirl.Virtual_register.t * Mirl.Virtual_register.t) Hashtbl.t

  let create () = Hashtbl.create (module Mirl.Virtual_register)

  let get_or_create map func_builder orig_reg =
    match Hashtbl.find map orig_reg with
    | Some pair -> pair
    | None ->
      (* Create a new register pair for this i64 register *)
      let low_reg = Mirl.Function.Builder.fresh_register func_builder ~ty:Mirl.Type.I32 in
      let high_reg =
        Mirl.Function.Builder.fresh_register func_builder ~ty:Mirl.Type.I32
      in
      Hashtbl.set map ~key:orig_reg ~data:(low_reg, high_reg);
      low_reg, high_reg
  ;;

  let get_pair map reg =
    match Hashtbl.find map reg with
    | Some pair -> pair
    | None -> reg, reg
  ;;
  (* For i32 registers, use the same register for both parts *)
end

(* Transform a single instruction, expanding i64 operations to register pairs *)
let legalize_instruction register_types pair_map func_builder instruction =
  let open Mirl.Instruction in
  match instruction with
  | Add { dst; src1; src2 } when Register_types.is_i64 register_types dst ->
    (* Transform i64 add into: ADD (low) + ADC (high with carry) *)
    let dst_low, dst_high = Register_pair_map.get_or_create pair_map func_builder dst in
    let src1_low, src1_high =
      Register_pair_map.get_or_create pair_map func_builder src1
    in
    let src2_low, src2_high =
      Register_pair_map.get_or_create pair_map func_builder src2
    in
    [ Add { dst = dst_low; src1 = src1_low; src2 = src2_low }
    ; Add_with_carry { dst = dst_high; src1 = src1_high; src2 = src2_high }
    ]
  | Sub { dst; src1; src2 } when Register_types.is_i64 register_types dst ->
    (* Transform i64 sub into: SUB (low) + SBC (high with carry/borrow) *)
    let dst_low, dst_high = Register_pair_map.get_or_create pair_map func_builder dst in
    let src1_low, src1_high =
      Register_pair_map.get_or_create pair_map func_builder src1
    in
    let src2_low, src2_high =
      Register_pair_map.get_or_create pair_map func_builder src2
    in
    [ Sub { dst = dst_low; src1 = src1_low; src2 = src2_low }
    ; Sub_with_carry { dst = dst_high; src1 = src1_high; src2 = src2_high }
    ]
  | Set { dst; value } when Register_types.is_i64 register_types dst ->
    (* Transform i64 immediate into two i32 immediates *)
    let dst_low, dst_high = Register_pair_map.get_or_create pair_map func_builder dst in
    let value_i64 = Int64.of_int value in
    let low_part = Int64.to_int_trunc (Int64.bit_and value_i64 0xFFFFFFFFL) in
    let high_part = Int64.to_int_trunc (Int64.shift_right value_i64 32) in
    [ Set { dst = dst_low; value = low_part }; Set { dst = dst_high; value = high_part } ]
  | Mov { dst; src } when Register_types.is_i64 register_types dst ->
    (* Transform i64 move into two i32 moves *)
    let dst_low, dst_high = Register_pair_map.get_or_create pair_map func_builder dst in
    let src_low, src_high = Register_pair_map.get_pair pair_map src in
    [ Mov { dst = dst_low; src = src_low }; Mov { dst = dst_high; src = src_high } ]
  | Return reg when Register_types.is_i64 register_types reg ->
    (* Transform i64 return: return low part of register pair *)
    let src_low, _src_high = Register_pair_map.get_pair pair_map reg in
    [ Return src_low ]
  | _ -> [ instruction ]
;;

(* Pass through i32 and other operations unchanged *)

(* Rebuild a function with legalized instructions *)
let legalize_function config func =
  if config.Config.supports_native_i64
  then func
  else (
    let register_types = Register_types.create func in
    Mirl.Function.build
      ~name:func.name
      ~params:(List.map func.params ~f:(fun (name, _, ty) -> name, ty))
      (fun func_builder params ->
        let pair_map = Register_pair_map.create () in
        (* Create parameter register mappings *)
        List.iter2_exn func.params params ~f:(fun (_, orig_reg, ty) (_, new_reg, _) ->
          match ty with
          | Mirl.Type.I64 ->
            (* For i64 parameters, create register pairs *)
            let low_reg =
              Mirl.Function.Builder.fresh_register func_builder ~ty:Mirl.Type.I32
            in
            let high_reg =
              Mirl.Function.Builder.fresh_register func_builder ~ty:Mirl.Type.I32
            in
            Hashtbl.set pair_map ~key:orig_reg ~data:(low_reg, high_reg)
          | Mirl.Type.I32 ->
            (* For i32 parameters, map to the new register *)
            Hashtbl.set pair_map ~key:orig_reg ~data:(new_reg, new_reg));
        (* Process each block *)
        Iarray.iter func.body ~f:(fun block ->
          Mirl.Function.Builder.add_block' func_builder (fun block_builder ->
            (* Process each instruction in the block *)
            Iarray.iter block.instructions ~f:(fun instruction ->
              let legalized_instructions =
                legalize_instruction register_types pair_map func_builder instruction
              in
              Mirl.Block.Builder.push_many block_builder legalized_instructions)
            [@nontail])
          [@nontail])
        [@nontail]))
;;

let legalize_program config program =
  if config.Config.supports_native_i64
  then program
  else
    let open Mirl in
    let legalized_functions = List.map program.functions ~f:(legalize_function config) in
    { program with functions = legalized_functions }
;;
