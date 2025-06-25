open! Core
open! Import
module Linscan = Mucaml_backend_common.Linscan.Make (Register)

module Registers = struct
  type t =
    { mapping : Register.t Virtual_register.Table.t
    ; used : Register.Set.t
    }

  let find_exn t register = Hashtbl.find_exn t.mapping register
  let find t register = Hashtbl.find t.mapping register
end

let label_name ~function_name ~label = [%string "%{function_name}__%{label#Mirl.Label}"]

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

let emit_block
  (block : Mirl.Block.t)
  buf
  ~registers
  ~function_name
  ~clobbered_callee_saved_registers
  =
  let open Arm_dsl in
  label buf (label_name ~function_name ~label:block.label);
  Iarray.iter block.instructions ~f:(fun instruction ->
    match instruction with
    | Add { dst; src1; src2 } ->
      let dst_reg = Registers.find_exn registers dst in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      add buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
    | Sub { dst; src1; src2 } ->
      let dst_reg = Registers.find_exn registers dst in
      let src1_reg = Registers.find_exn registers src1 in
      let src2_reg = Registers.find_exn registers src2 in
      sub buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
    | Set { dst; value } ->
      let dst_reg = Registers.find_exn registers dst in
      mov_imm buf ~dst:dst_reg (I32.of_int value)
    | Mov { dst; src } ->
      let dst_reg = Registers.find_exn registers dst in
      let src_reg = Registers.find_exn registers src in
      mov buf ~dst:dst_reg ~src:src_reg
    | C_call { dst; func; args } ->
      let dst_reg = Registers.find registers dst in
      let args_regs = List.map args ~f:(Registers.find_exn registers) in
      c_call buf ~dst:dst_reg ~func ~args:args_regs
    | Return reg ->
      let reg = Registers.find_exn registers reg in
      if not (Register.equal reg W0) then mov buf ~dst:W0 ~src:reg;
      if not (List.is_empty clobbered_callee_saved_registers)
      then pop buf clobbered_callee_saved_registers;
      ret buf
    | Jump { target } -> b buf ~target:(label_name ~function_name ~label:target)
    | Branch { condition; target } ->
      let condition_reg = Registers.find_exn registers condition in
      tbnz buf ~condition:condition_reg ~target:(label_name ~function_name ~label:target))
;;

let emit_function (func : Mirl.Function.t) buf =
  let open Arm_dsl in
  let registers : Registers.t =
    let mapping, used = Linscan.allocate_registers func in
    { mapping; used }
  in
  let clobbered_callee_saved_registers =
    Set.inter registers.used Register.callee_saved |> Set.to_list
  in
  emit_function_prologue buf ~name:func.name;
  if not (List.is_empty clobbered_callee_saved_registers)
  then push buf clobbered_callee_saved_registers;
  Iarray.iter func.body ~f:(fun block ->
    emit_block
      block
      buf
      ~registers
      ~function_name:func.name
      ~clobbered_callee_saved_registers);
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
