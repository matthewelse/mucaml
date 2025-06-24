open! Core
open! Import
module Linscan = Mucaml_backend_common.Linscan.Make (Register)

module Registers = struct
  type t =
    { mapping : Register.t Mirl.Register.Table.t
    ; used : Register.Set.t
    }

  let find_exn t register = Hashtbl.find_exn t.mapping register
  let find t register = Hashtbl.find t.mapping register
end

let c_call buf ~dst ~func ~args =
  let open Arm_dsl in
  (* TODO: only push r0/r1/r2 if they are live after the call *)
  push buf (List.mapi args ~f:(fun i _ -> Iarray.get Register.function_args i));
  List.iteri args ~f:(fun i arg ->
    let reg = Iarray.get Register.function_args i in
    if not (Register.equal reg arg) then mov buf ~dst:reg ~src:arg);
  bl buf ~func;
  (match dst with
   | None -> ()
   | Some dst ->
     if not (Register.equal dst Register.return_register)
     then mov buf ~dst ~src:Register.return_register);
  pop buf (List.mapi args ~f:(fun i _ -> Iarray.get Register.function_args i))
;;

let emit_block (block : Mirl.Block.t) buf ~registers =
  let open Arm_dsl in
  Iarray.iter block.instructions ~f:(fun instruction ->
    match instruction with
    | Add { dest; src1; src2 } ->
      let dest_reg = Registers.find_exn registers dest in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      add buf ~dst:dest_reg ~src1:src1_reg ~src2:src2_reg
    | Sub { dest; src1; src2 } ->
      let dest_reg = Registers.find_exn registers dest in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      sub buf ~dst:dest_reg ~src1:src1_reg ~src2:src2_reg
    | Set { dest; value } ->
      let dest_reg = Registers.find_exn registers dest in
      mov_imm buf ~dst:dest_reg (I32.of_int value)
    | Mov { dest; src } ->
      let dest_reg = Registers.find_exn registers dest in
      let src_reg = Registers.find_exn registers src in
      mov buf ~dst:dest_reg ~src:src_reg
    | C_call { dest; func; args } ->
      let dest_reg = Registers.find registers dest in
      let args_regs = List.map args ~f:(Registers.find_exn registers) in
      c_call buf ~dst:dest_reg ~func ~args:args_regs);
  match block.terminator with
  | Return reg ->
    let reg = Registers.find_exn registers reg in
    if not (Register.equal reg W0) then mov buf ~dst:W0 ~src:reg;
    ret buf
;;

let emit_function (func : Mirl.Function.t) buf =
  let open Arm_dsl in
  let registers : Registers.t =
    let mapping, used = Linscan.allocate_registers func.body ~inputs:[] in
    { mapping; used }
  in
  let clobbered_callee_saved_registers =
    Set.inter registers.used Register.callee_saved |> Set.to_list
  in
  emit_function_prologue buf ~name:func.name;
  if not (List.is_empty clobbered_callee_saved_registers)
  then push buf clobbered_callee_saved_registers;
  emit_block func.body buf ~registers;
  if not (List.is_empty clobbered_callee_saved_registers)
  then pop buf clobbered_callee_saved_registers;
  emit_function_epilogue buf ~name:func.name
;;

let emit_cmm_without_prologue (program : Mirl.t) buf =
  List.iter program.functions ~f:(fun func -> emit_function func buf);
  Arm_dsl.to_string buf
;;

let emit_cmm (program : Mirl.t) =
  let open Arm_dsl in
  let buf = Arm_dsl.create () in
  emit_program_prologue buf;
  emit_cmm_without_prologue program buf
;;

module For_testing = struct
  let emit_cmm_without_prologue program =
    let buf = Arm_dsl.create () in
    emit_cmm_without_prologue program buf
  ;;
end
