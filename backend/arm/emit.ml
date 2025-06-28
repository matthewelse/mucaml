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

let c_call buf ~dst ~func ~args ~clobbered_caller_saved_registers =
  let open Arm_dsl in
  if not (List.is_empty clobbered_caller_saved_registers)
  then push buf clobbered_caller_saved_registers;
  List.iteri args ~f:(fun i arg ->
    let reg = Iarray.get Register.function_args i in
    if not (Register.equal reg arg) then mov buf ~dst:reg ~src:arg);
  bl buf ~func;
  (match dst with
   | None -> ()
   | Some dst ->
     if not (Register.equal dst Register.return_register)
     then mov buf ~dst ~src:Register.return_register);
  if not (List.is_empty clobbered_caller_saved_registers)
  then pop buf clobbered_caller_saved_registers
;;

let emit_block
  (block : Mirl.Block.t)
  buf
  ~registers
  ~function_name
  ~clobbered_callee_saved_registers
  ~call_sites
  =
  let open Arm_dsl in
  label buf (label_name ~function_name ~label:block.label);
  Iarray.iteri block.instructions ~f:(fun insn_idx instruction ->
    match instruction with
    | Add { dst; src1; src2 } ->
      (match Registers.find registers dst with
       | Some dst_reg ->
         let src1_reg = Registers.find_exn registers src1 in
         let src2_reg = Registers.find_exn registers src2 in
         add buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
       | None -> (* Destination register is unused, skip instruction *) ())
    | Sub { dst; src1; src2 } ->
      (match Registers.find registers dst with
       | Some dst_reg ->
         let src1_reg = Registers.find_exn registers src1 in
         let src2_reg = Registers.find_exn registers src2 in
         sub buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
       | None -> (* Destination register is unused, skip instruction *) ())
    | Add_with_carry { dst; src1; src2 } ->
      (match Registers.find registers dst with
       | Some dst_reg ->
         let src1_reg = Registers.find_exn registers src1 in
         let src2_reg = Registers.find_exn registers src2 in
         adc buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
       | None -> (* Destination register is unused, skip instruction *) ())
    | Sub_with_carry { dst; src1; src2 } ->
      (match Registers.find registers dst with
       | Some dst_reg ->
         let src1_reg = Registers.find_exn registers src1 in
         let src2_reg = Registers.find_exn registers src2 in
         sbc buf ~dst:dst_reg ~src1:src1_reg ~src2:src2_reg
       | None -> (* Destination register is unused, skip instruction *) ())
    | Set { dst; value } ->
      (match Registers.find registers dst with
       | Some dst_reg -> mov_imm buf ~dst:dst_reg (I32.of_int value)
       | None -> (* Register is unused, skip instruction *) ())
    | Mov { dst; src } ->
      (match Registers.find registers dst with
       | Some dst_reg ->
         let src_reg = Registers.find_exn registers src in
         if not (Register.equal dst_reg src_reg) then mov buf ~dst:dst_reg ~src:src_reg
       | None -> (* Destination register is unused, skip instruction *) ())
    | C_call { dst; func; args } ->
      let dst_reg = Registers.find registers dst in
      let args_regs = List.map args ~f:(Registers.find_exn registers) in
      let clobbered_caller_saved_registers =
        List.find_map
          call_sites
          ~f:(fun (~block:this_block, ~insn_idx:this_insn_idx, ~live_regs) ->
            if Mirl.Label.equal this_block block.label && insn_idx = this_insn_idx
            then Some live_regs
            else None)
        |> Option.value_map ~default:[] ~f:Set.to_list
      in
      c_call buf ~dst:dst_reg ~func ~args:args_regs ~clobbered_caller_saved_registers
    | Return regs ->
      (match regs with
       | [reg] -> 
         (* Single register return (i32) *)
         let reg = Registers.find_exn registers reg in
         if not (Register.equal reg R0) then mov buf ~dst:R0 ~src:reg
       | [low_reg; high_reg] ->
         (* Two register return (i64) - ARM32 ABI: r0=low, r1=high *)
         let low_phys = Registers.find_exn registers low_reg in
         let high_phys = Registers.find_exn registers high_reg in
         if not (Register.equal low_phys R0) then mov buf ~dst:R0 ~src:low_phys;
         if not (Register.equal high_phys R1) then mov buf ~dst:R1 ~src:high_phys
       | _ -> failwith "ARM32 return supports 1 or 2 registers only");
      pop buf (PC :: clobbered_callee_saved_registers)
    | Jump { target } -> b buf ~target:(label_name ~function_name ~label:target)
    | Branch { condition; target } ->
      let condition_reg = Registers.find_exn registers condition in
      cbnz buf ~condition:condition_reg ~target:(label_name ~function_name ~label:target))
;;

let emit_function (func : Mirl.Function.t) buf =
  let open Arm_dsl in
  let mapping, used, call_sites = Linscan.allocate_registers func in
  let registers : Registers.t = { mapping; used } in
  let clobbered_callee_saved_registers =
    Set.inter registers.used Register.callee_saved |> Set.to_list
  in
  emit_function_prologue buf ~name:func.name;
  (* TODO: Properly track whether or not LR is actually clobbered (i.e. whether or not we have
     a bl somewhere in this function). *)
  push buf (LR :: clobbered_callee_saved_registers);
  List.iteri func.params ~f:(fun i (_, reg, _) ->
    match Registers.find registers reg with
    | None -> (* This arg is never used, ignore it. *) ()
    | Some reg ->
      let conv_reg = Iarray.get Register.function_args i in
      if not (Register.equal reg conv_reg) then mov buf ~dst:reg ~src:conv_reg);
  Iarray.iter func.body ~f:(fun block ->
    emit_block
      block
      buf
      ~registers
      ~function_name:func.name
      ~clobbered_callee_saved_registers
      ~call_sites);
  emit_function_epilogue buf ~name:func.name
;;

let emit_mirl_without_prologue (program : Mirl.t) buf =
  List.iter program.functions ~f:(fun func -> emit_function func buf);
  Arm_dsl.to_string buf
;;

let emit_mirl (program : Mirl.t) =
  let open Arm_dsl in
  let buf = Arm_dsl.create () in
  emit_program_prologue buf;
  emit_mirl_without_prologue program buf
;;

module For_testing = struct
  let emit_mirl_without_prologue program =
    let buf = Arm_dsl.create () in
    emit_mirl_without_prologue program buf
  ;;
end
